use darling::{FromField, FromAttributes, FromVariant, FromMeta};
use proc_macro2::{TokenStream, Span};
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Type, Ident, parse_str, Path, Fields, Field, GenericParam};

#[derive(Debug, FromAttributes)]
#[darling(attributes(marshal))]
struct StructOrEnumReceiver {
  magic: Option<syn::LitByteStr>,
  ctx: Option<Path>,
  tag_type: Option<Path>,
  tag: Option<String>,
  tag_bits: Option<usize>
}

#[derive(Debug, Clone, Copy, FromMeta)]
enum CtxType {
  Coerce,
  Forward,
  Construct
}

#[derive(Debug, FromMeta, PartialEq, Eq)]
struct ContextMapping {
  field: String,
  member: String,
}

#[derive(Debug, FromField)]
#[darling(attributes(marshal))]
struct StructFieldReceiver {
  ident: Option<Ident>,
  ty: Type,

  align: Option<usize>,
  bits: Option<usize>,
  ctx: Option<CtxType>,
  ctx_type: Option<Path>,
  #[darling(multiple, rename = "ctx_member")]
  ctx_members: Vec<ContextMapping>
}

#[derive(Debug, FromVariant)]
#[darling(attributes(marshal))]
struct EnumVariantReceiver {
  ident: Ident,
  
  tag: String
}

struct ProcessedField {
  var_name: Ident,

  ty: Type,
  receiver: StructFieldReceiver,

  context_type: TokenStream,
  context_body: TokenStream,

  get_ref: TokenStream,
  get_ref_mut: TokenStream,
  construct: TokenStream,
}

struct MarshalProcessedField {
  pf: ProcessedField,
  write_body: TokenStream,
}

struct DemarshalProcessedField {
  pf: ProcessedField,
  read_body: TokenStream,
}

struct UpdateProcessedField {
  pf: ProcessedField,
  update_body: Option<TokenStream>
}

fn process_field(our_context_type: &TokenStream, i: usize, field: &Field) -> ProcessedField {
  let idx = syn::Index::from(i);

  let receiver = StructFieldReceiver::from_field(field).unwrap();

  let (accessor, var_name) = match receiver.ident.clone() {
    Some(ident) => (quote! { #ident }, ident),
    None => (quote! { #idx }, syn::Ident::new(format!("_{}", i).as_str(), Span::call_site()))
  };

  let ty = receiver.ty.clone();

  let (ctx_ty, ctx_val) = match (receiver.bits, receiver.ctx.as_ref(), receiver.ctx_type.as_ref()) {
    (Some(bits), _, _) => ( quote!{ binmarshal::BitSpecification::<#bits> }, quote!{ binmarshal::BitSpecification::<#bits> } ),
    (_, Some(CtxType::Forward), _) => ( quote!{ #our_context_type }, quote!{ ctx } ),
    (_, Some(CtxType::Coerce), Some(ctx_type)) => ( quote!{ #ctx_type }, quote!{ ctx.into() } ),
    (_, Some(CtxType::Construct), Some(ctx_type)) => {
      let inner = receiver.ctx_members.iter().map(|x| {
        let field: TokenStream = parse_str(&x.field).unwrap();
        let member: TokenStream = parse_str(&x.member).unwrap();
        quote!{ #field: #member.clone() }
      });
      (quote!{ #ctx_type }, quote!{ #ctx_type { #(#inner),* } })
    },
    (_, None, _) => {
      (quote!{ () }, quote!{ () })
    },
    _ => panic!("Invalid Context Combination")
  };

  ProcessedField {
    get_ref: quote!{ let #var_name = &self.#accessor },
    get_ref_mut: quote!{ let #var_name = &mut self.#accessor },
    construct: quote!{ #var_name },
    var_name,
    ty,
    receiver,
    context_type: ctx_ty,
    context_body: ctx_val,
  }
}

fn process_field_marshal(our_context_type: &TokenStream, i: usize, field: Field) -> MarshalProcessedField {
  let pf = process_field(our_context_type, i, &field);
  let ProcessedField { var_name, ty, receiver, context_type, context_body, .. } = &pf;

  let align = match receiver.align {
    Some(align) => quote!{ writer.align(#align) },
    None => quote!{}
  };

  let write_body = quote! {
    {
      #align;
      <#ty as binmarshal::Marshal<#context_type>>::write(#var_name, writer, #context_body)
    }?
  };

  MarshalProcessedField { pf, write_body }
}

fn process_field_demarshal(our_context_type: &TokenStream, i: usize, field: Field) -> DemarshalProcessedField {
  let pf = process_field(our_context_type, i, &field);
  let ProcessedField { var_name, ty, receiver, context_type, context_body, .. } = &pf;

  let align = match receiver.align {
    Some(align) => quote!{ view.align(#align) },
    None => quote!{}
  };

  let read_body = quote! {
    let #var_name = {
      #align;
      <#ty as binmarshal::Demarshal<'dm, #context_type>>::read(view, #context_body)
    }?;
  };

  DemarshalProcessedField { pf, read_body }
}

fn process_field_update(our_context_type: &TokenStream, i: usize, field: Field) -> UpdateProcessedField {
  let pf = process_field(our_context_type, i, &field);
  let ProcessedField { var_name, receiver, .. } = &pf;

  let update_body = match receiver.ctx.as_ref() {
    Some(CtxType::Forward) => Some(quote! { #var_name.update(ctx) }),
    Some(CtxType::Coerce) => Some(quote! {
      let mut new_context = ctx.clone().into();
      #var_name.update(&mut new_context);
      *ctx = new_context.into();
    }),
    Some(CtxType::Construct) => {
      let inner = receiver.ctx_members.iter().map(|x| {
        let field: TokenStream = parse_str(&x.field).unwrap();
        let member: TokenStream = parse_str(&x.member).unwrap();

        // Don't deref if it'll get implicitly derefed through indirection
        let deref = match x.member.contains(".") {
            true => quote!{ #member },
            false => quote!{ *#member },
        };
        (quote!{ #field: #deref }, quote!{ #deref = new_context.#field })
      });
      let create_new_items = inner.clone().map(|x| x.0);
      let propagate_back = inner.clone().map(|x| x.1);

      let ctx_type = receiver.ctx_type.as_ref().unwrap();

      Some(quote!{
        let mut new_context = #ctx_type { #(#create_new_items),* };
        #var_name.update(&mut new_context);
        #(#propagate_back);*
      })
    },
    _ => None
  };

  UpdateProcessedField { pf, update_body }
}

fn strip_bounds<'a, I: Iterator<Item = &'a GenericParam>>(generics: I) -> TokenStream {
  let g = generics.map(|x| match x {
    syn::GenericParam::Lifetime(lt) => {
      let i = &lt.lifetime;
      quote!{ #i }
    },
    syn::GenericParam::Type(ty) => {
      let i = &ty.ident;
      quote!{ #i }
    },
    syn::GenericParam::Const(c) => {
      let c2 = &c.ident;
      quote!{ #c2 }
    },
  });
  quote!{ #(#g),* }
}

// Marshal

#[proc_macro_derive(Marshal, attributes(marshal))]
pub fn derive_marshal(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let DeriveInput {
    attrs, vis: _, ident, generics, data
  } = parse_macro_input!(input as DeriveInput);

  let generics_without_bounds = strip_bounds(generics.params.iter());

  let attrs = StructOrEnumReceiver::from_attributes(&attrs).unwrap();
  let ctx_ty = if let Some(ctx) = attrs.ctx {
    quote!{ #ctx }
  } else {
    quote! { () }
  };

  let magic_write = match attrs.magic {
    Some(lit) => {
      Some(quote!{
        writer.write_magic(#lit)?
      })
    },
    None => None
  };

  match data {
    syn::Data::Struct(st) => {
      let it = st.fields.into_iter().enumerate().map(|(i, field)| process_field_marshal(&ctx_ty, i, field));

      let to_write = it.clone().map(|x| x.write_body);
      let refs = it.clone().map(|x| x.pf.get_ref);

      let out = quote! {
        impl #generics binmarshal::Marshal<#ctx_ty> for #ident<#generics_without_bounds> {
          fn write<W: binmarshal::rw::BitWriter>(&self, writer: &mut W, ctx: #ctx_ty) -> core::result::Result<(), binmarshal::MarshalError> {
            #magic_write;

            #(#refs;)*
            #(#to_write;)*
            Ok(())
          }
        }
      };

      out.into()
    },
    syn::Data::Enum(en) => {
      let write_tag = match &attrs.tag {
        Some(_) => quote! { },    // It'll get propagated through the context
        None => {
          // We need to serialise the tag
          let tag_type = attrs.tag_type.clone().unwrap();

          let ctx_val = match attrs.tag_bits {
            Some(bits) => quote!{ binmarshal::BitSpecification::<#bits> {} },
            None => quote! { () }
          };

          quote! {
            <#tag_type as binmarshal::Marshal<_>>::write(&_tag, writer, #ctx_val)?
          }
        }
      };

      let it = en.variants.into_iter().map(|variant| {
        let receiver = EnumVariantReceiver::from_variant(&variant).unwrap();
        let name = receiver.ident;
        let tag: TokenStream = parse_str(&receiver.tag).unwrap();

        let (fields, is_paren) = match variant.fields {
          Fields::Named(named) => (named.named.into_iter().collect(), false),
          Fields::Unnamed(unnamed) => (unnamed.unnamed.into_iter().collect(), true),
          Fields::Unit => (vec![], false),
        };

        let processed_fields = fields.into_iter().enumerate().map(|(i, field)| process_field_marshal(&ctx_ty, i, field));

        let to_write = processed_fields.clone().map(|t| t.write_body);
        let construct = processed_fields.clone().map(|t| t.pf.construct);

        let write_tag = quote!{
          Self::#name { .. } => #tag
        };

        let write = match is_paren {
          true => quote!{
            Self::#name ( #(#construct),* ) => {
              #(#to_write;)*
            }
          },
          false => quote!{
            Self::#name { #(#construct),* } => {
              #(#to_write;)*
            }
          },
        };

        (write_tag, write)
      });

      let write_tag_variants = it.clone().map(|v| v.0);
      let write_variants = it.clone().map(|v| v.1);

      let out = quote! {
        impl #generics binmarshal::Marshal<#ctx_ty> for #ident<#generics_without_bounds> {
          #[inline(always)]
          #[allow(unused_variables)]
          fn write<W: binmarshal::rw::BitWriter>(&self, writer: &mut W, ctx: #ctx_ty) -> core::result::Result<(), binmarshal::MarshalError> {
            let _tag = match &self {
              #(#write_tag_variants),*
            };
            #magic_write;
            #write_tag;
            match self {
              #(#write_variants),*
            };

            Ok(())
          }
        }
      };

      out.into()
    },
    syn::Data::Union(_) => panic!("Don't know how to serialise unions!"),
  }
}

// Demarshal

#[proc_macro_derive(Demarshal, attributes(marshal))]
pub fn derive_demarshal(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let DeriveInput {
    attrs, vis: _, ident, generics, data
  } = parse_macro_input!(input as DeriveInput);

  let generics_inner = generics.params.iter();
  let generics_inner = quote!{ #(#generics_inner),* };
  let generics_without_bounds = strip_bounds(generics.params.iter());

  let attrs = StructOrEnumReceiver::from_attributes(&attrs).unwrap();
  let ctx_ty = if let Some(ctx) = attrs.ctx {
    quote!{ #ctx }
  } else {
    quote! { () }
  };

  let magic_read = match attrs.magic {
    Some(lit) => {
      Some(quote!{
        view.check_magic(#lit)?
      })
    },
    None => None
  };

  match data {
    syn::Data::Struct(st) => {
      let it = st.fields.into_iter().enumerate().map(|(i, field)| process_field_demarshal(&ctx_ty, i, field));

      let to_read = it.clone().map(|x| x.read_body);
      let construct = it.clone().map(|x| x.pf.construct);

      let out = quote! {
        impl <'dm, #generics_inner> binmarshal::Demarshal<'dm, #ctx_ty> for #ident<#generics_without_bounds> {
          fn read(view: &mut binmarshal::rw::BitView<'dm>, ctx: #ctx_ty) -> core::result::Result<Self, binmarshal::MarshalError> {
            #magic_read;

            #(#to_read)*
            Ok(Self {
              #(#construct),*
            })
          }
        }
      };

      out.into()
    },
    syn::Data::Enum(en) => {
      let read_tag = match &attrs.tag {
        Some(tag) => {
          let in_tag: TokenStream = parse_str(&tag).unwrap();
          quote! { let _tag = #in_tag; }
        },
        None => {
          // We need to deserialise the tag
          let tag_type = attrs.tag_type.clone().unwrap();

          let ctx_val = match attrs.tag_bits {
            Some(bits) => quote!{ binmarshal::BitSpecification::<#bits> {} },
            None => quote! { () }
          };

          quote! {
            let _tag = <#tag_type as binmarshal::Demarshal<'dm, _>>::read(view, #ctx_val)?;
          }
        }
      };

      let read_variants = en.variants.into_iter().map(|variant| {
        let receiver = EnumVariantReceiver::from_variant(&variant).unwrap();
        let name = receiver.ident;
        let tag: TokenStream = parse_str(&receiver.tag).unwrap();

        let (fields, is_paren) = match variant.fields {
          Fields::Named(named) => (named.named.into_iter().collect(), false),
          Fields::Unnamed(unnamed) => (unnamed.unnamed.into_iter().collect(), true),
          Fields::Unit => (vec![], false),
        };

        let processed_fields = fields.into_iter().enumerate().map(|(i, field)| process_field_demarshal(&ctx_ty, i, field));

        let to_read = processed_fields.clone().map(|t| t.read_body);
        let construct = processed_fields.clone().map(|t| t.pf.construct);

        let read = match is_paren {
          true => quote!{
            (#tag) => {
              #(#to_read)*
              Ok(Self::#name(#(#construct),*))
            }
          },
          false => quote!{
            (#tag) => {
              #(#to_read)*
              Ok(Self::#name { #(#construct),*})
            }
          },
        };

        read
      });

      let out = quote! {
        impl<'dm, #generics_inner> binmarshal::Demarshal<'dm, #ctx_ty> for #ident<#generics_without_bounds> {
          #[inline(always)]
          #[allow(unused_variables)]
          fn read(view: &mut binmarshal::rw::BitView<'dm>, ctx: #ctx_ty) -> core::result::Result<Self, binmarshal::MarshalError> {
            #magic_read;
            #read_tag;
            match _tag {
              #(#read_variants),*,
              _ => Err(binmarshal::MarshalError::IllegalTag)
            }
          }
        }
      };

      out.into()
    },
    syn::Data::Union(_) => panic!("Don't know how to serialise unions!"),
  }
}

// Update

#[proc_macro_derive(MarshalUpdate, attributes(marshal))]
pub fn derive_marshal_update(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let DeriveInput {
    attrs, vis: _, ident, generics, data
  } = parse_macro_input!(input as DeriveInput);

  let generics_without_bounds = strip_bounds(generics.params.iter());

  let attrs = StructOrEnumReceiver::from_attributes(&attrs).unwrap();
  let ctx_ty = if let Some(ctx) = attrs.ctx {
    quote!{ #ctx }
  } else {
    quote! { () }
  };

  match data {
    syn::Data::Struct(st) => {
      let it = st.fields.into_iter().enumerate().map(|(i, field)| process_field_update(&ctx_ty, i, field));

      let to_update = it.clone().map(|x| x.update_body);
      let get_ref_mut = it.clone().map(|x| x.pf.get_ref_mut);

      let out = quote! {
        impl #generics binmarshal::MarshalUpdate<#ctx_ty> for #ident<#generics_without_bounds> {
          fn update(&mut self, ctx: &mut #ctx_ty) {
            #(#get_ref_mut;)*
            #(#to_update;)*
          }
        }
      };

      out.into()
    },
    syn::Data::Enum(en) => {
      let update_tag = match &attrs.tag {
        Some(tag) => {
          let in_tag: TokenStream = parse_str(&tag).unwrap();
          Some(in_tag)
        },
        None => None
      };

      let update_variants = en.variants.into_iter().map(|variant| {
        let receiver = EnumVariantReceiver::from_variant(&variant).unwrap();
        let name = receiver.ident;
        let tag: TokenStream = parse_str(&receiver.tag).unwrap();

        let inner_update: TokenStream = update_tag.clone().map(|x| quote!{ #x = #tag }).unwrap_or(quote!{ });

        let (fields, is_paren) = match variant.fields {
          Fields::Named(named) => (named.named.into_iter().collect(), false),
          Fields::Unnamed(unnamed) => (unnamed.unnamed.into_iter().collect(), true),
          Fields::Unit => (vec![], false),
        };

        let processed_fields = fields.into_iter().enumerate().map(|(i, field)| process_field_update(&ctx_ty, i, field));

        let to_update = processed_fields.clone().map(|t| t.update_body);
        let construct = processed_fields.clone().map(|t| t.pf.construct);

        let update = match is_paren {
          true => quote! {
            Self::#name(#(#construct),*) => {
              #inner_update;
              #(#to_update;)*
            }
          },
          false => quote! {
            Self::#name { #(#construct),* } => {
              #inner_update;
              #(#to_update;)*
            }
          },
        };

        update
      });

      let out = quote! {
        impl #generics binmarshal::MarshalUpdate<#ctx_ty> for #ident<#generics_without_bounds> {
          fn update(&mut self, ctx: &mut #ctx_ty) {
            match self {
              #(#update_variants),*
            }
          }
        }
      };

      out.into()
    },
    syn::Data::Union(_) => panic!("Don't know how to serialise unions!"),
  }
}

// PROXY TYPES

#[proc_macro_derive(Proxy)]
pub fn derive_proxy(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let DeriveInput {
    attrs: _, vis: _, ident, generics, data
  } = parse_macro_input!(input as DeriveInput);

  let generics_inner = &generics.params;

  match data {
    syn::Data::Struct(st) => {
      match st.fields {
        Fields::Unnamed(fields) => {
          let field = &fields.unnamed[0];
          let mut extra_default = vec![];
          let mut extra_clone = vec![];

          for (_, _) in fields.unnamed.iter().enumerate().skip(1) {
            extra_default.push(quote! { Default::default() });
          }

          for (i, _) in fields.unnamed.iter().enumerate().skip(1) {
            let i = syn::Index::from(i);
            extra_clone.push(quote! { self.#i.clone() });
          }

          let ft = &field.ty;

          let ident_generics = generics.params.iter().map(|g| match g {
            syn::GenericParam::Lifetime(lt) => {
              let lt = &lt.lifetime;
              quote!{ #lt }
            },
            syn::GenericParam::Type(ty) => {
              let t = &ty.ident;
              quote!{ #t }
            },
            syn::GenericParam::Const(c) => {
              let t = &c.ident;
              quote!{ #t }
            },
          });

          let (lt, gt) = (generics.lt_token, generics.gt_token);
          let ident_generics = quote! {
            #lt #(#ident_generics),* #gt
          };

          let mut out = quote! {
            impl #generics From<#ft> for #ident #ident_generics {
              fn from(inner: #ft) -> Self {
                Self(inner, #(#extra_default),*)
              }
            }

            impl #generics Deref for #ident #ident_generics {
              type Target = #ft;

              fn deref(&self) -> &Self::Target { &self.0 }
            }

            impl #generics DerefMut for #ident #ident_generics {
              fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
            }

            impl #generics core::fmt::Debug for #ident #ident_generics where #ft: core::fmt::Debug {
              fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result { self.0.fmt(f) }
            }

            impl #generics Clone for #ident #ident_generics where #ft: Clone {
              fn clone(&self) -> Self { Self(self.0.clone(),#(#extra_clone),*) }
            }

            impl #generics PartialEq for #ident #ident_generics where #ft: PartialEq {
               #[inline]
                fn eq(&self, other: &Self) -> bool {
                    PartialEq::eq(&self.0, &other.0)
                }
                #[inline]
                fn ne(&self, other: &Self) -> bool {
                    PartialEq::ne(&self.0, &other.0)
                }
            }

            impl #generics Eq for #ident #ident_generics where #ft: Eq { }
          };

          #[cfg(feature = "serde")]
          {
            out = quote!{
              #out

              impl #generics serde::Serialize for #ident #ident_generics where #ft: serde::Serialize {
                  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                  where
                    S: serde::Serializer,
                  {
                    self.0.serialize(serializer)
                  }
              }

              impl<'de, #generics_inner> serde::Deserialize<'de> for #ident #ident_generics where #ft: serde::Deserialize<'de> {
                  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                  where
                      D: serde::Deserializer<'de>,
                  {
                    <#ft as serde::Deserialize<'de>>::deserialize::<D>(deserializer).map(|x| Self(x, #(#extra_default),*))
                  }
              }
            };
          }

          #[cfg(feature = "schema")]
          {
            out = quote!{
              #out 

              impl #generics schemars::JsonSchema for #ident #ident_generics where #ft: schemars::JsonSchema {
                fn schema_name() -> alloc::string::String {
                  <#ft as schemars::JsonSchema>::schema_name()
                }

                fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
                  <#ft as schemars::JsonSchema>::json_schema(gen)
                }

                fn is_referenceable() -> bool {
                  <#ft as schemars::JsonSchema>::is_referenceable()
                }
              }
            }
          }

          out.into()
        },
        _ => panic!("Proxy only supported on newtype structs"),
      }
    },
    syn::Data::Enum(_) => panic!("Proxy not supported on Enum types"),
    syn::Data::Union(_) => panic!("Proxy not supported on Union types"),
  }
}