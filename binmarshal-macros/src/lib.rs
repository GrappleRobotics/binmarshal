use darling::{FromField, FromAttributes, FromVariant, FromMeta};
use proc_macro2::{TokenStream, Span};
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Type, Ident, parse_str, Path, Fields, Field, parse::Parse, Token};

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
  #[darling(multiple, rename = "ctx_member")]
  ctx_members: Vec<ContextMapping>
}

#[derive(Debug, FromVariant)]
#[darling(attributes(marshal))]
struct EnumVariantReceiver {
  ident: Ident,
  
  tag: String
}

fn process_struct_field(i: usize, field: Field) -> (TokenStream, TokenStream, TokenStream, TokenStream, TokenStream, TokenStream, Option<TokenStream>) {
  let idx = syn::Index::from(i);

  let receiver = StructFieldReceiver::from_field(&field).unwrap();

  let (accessor, var_name) = match receiver.ident {
    Some(ident) => (quote! { #ident }, ident),
    None => (quote! { #idx }, syn::Ident::new(format!("_{}", i).as_str(), Span::call_site()))
  };

  let ty = receiver.ty;

  let ctx_val = match (receiver.bits, receiver.ctx.as_ref()) {
    (Some(bits), _) => quote!{ binmarshal::BitSpecification::<#bits> },
    (_, Some(CtxType::Forward)) => {
      quote!{ ctx }
    },
    (_, Some(CtxType::Coerce)) => {
      quote!{ ctx.into() }
    },
    (_, Some(CtxType::Construct)) => {
      let inner = receiver.ctx_members.iter().map(|x| {
        let field: TokenStream = parse_str(&x.field).unwrap();
        let member: TokenStream = parse_str(&x.member).unwrap();
        quote!{ #field: #member.clone() }
      });
      quote!{ binmarshal::SelfType::<<#ty as binmarshal::BinMarshal<_>>::Context> { #(#inner),* } }
    },
    (_, None) => {
      quote!{ () }
    }
  };

  let (align_write, align_read) = receiver.align.map(|align| (quote!{ writer.align(#align) }, quote!{ view.align(#align) })).unwrap_or((quote!{ true }, quote!{ true }));

  let write = quote! {
    (#align_write) && (<#ty as binmarshal::BinMarshal<_>>::write(#var_name, writer, #ctx_val))
  };

  let read = quote! {
    #align_read;
    let #var_name = <#ty as binmarshal::BinMarshal<_>>::read(view, #ctx_val)?;
  };

  let unpack = quote! {
    let #var_name = &self.#accessor;
  };

  let unpack_mutable = quote! {
    let #var_name = &mut self.#accessor;
  };

  let construct = quote! { #var_name };
  let destruct = quote!{ #var_name };

  let update = match receiver.ctx.as_ref() {
    Some(CtxType::Forward) => Some(quote!{ #var_name.update(ctx) }),
    Some(CtxType::Coerce) => {
      Some(quote! {
        let mut new_context = ctx.clone().into();
        #var_name.update(&mut new_context);
        *ctx = new_context.into();
      })
    },
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

      Some(quote!{
        let mut new_context = binmarshal::SelfType::<<#ty as binmarshal::BinMarshal<_>>::Context> { #(#create_new_items),* };
        #var_name.update(&mut new_context);
        #(#propagate_back);*
      })
    }
    _ => None
  };

  (write, read, construct, destruct, unpack, unpack_mutable, update)
}

#[proc_macro_derive(BinMarshal, attributes(marshal))]
pub fn derive_bin_marshal(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let DeriveInput {
    attrs, vis: _, ident, generics, data
  } = parse_macro_input!(input as DeriveInput);

  let generics_without_bounds = generics.params.iter().map(|x| match x {
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

  let attrs = StructOrEnumReceiver::from_attributes(&attrs).unwrap();
  let ctx_ty = if let Some(ctx) = attrs.ctx {
    quote!{ #ctx }
  } else {
    quote! { () }
  };

  let (magic_definition, magic_write, magic_read) = match attrs.magic {
    Some(lit) => {
      let magic_name = syn::Ident::new(&format!("{}Magic", ident), ident.span());
      (gen_magic(Magic { name: magic_name.clone(), magic: lit }).into(), quote!{ #magic_name {}.write(writer, ()) }, quote! { <#magic_name as binmarshal::BinMarshal<()>>::read(view, ())?; })
    },
    None => (quote!{}, quote!{ true }, quote!{})
  };

  match data {
    syn::Data::Struct(st) => {
      let it = st.fields.into_iter().enumerate().map(|(i, field)| process_struct_field(i, field));

      let to_write = it.clone().map(|x| x.0).reduce(|a, b| quote!{ #a && #b });
      let to_read = it.clone().map(|x| x.1);
      let to_construct = it.clone().map(|x| x.2);
      let to_unpack = it.clone().map(|x| x.4);
      let to_unpack_mutable = it.clone().map(|x| x.5);
      let to_update = it.clone().map(|x| x.6).filter_map(|x| x);

      let out = quote! {
        #magic_definition

        impl #generics binmarshal::BinMarshal<#ctx_ty> for #ident<#(#generics_without_bounds),*>  {
          type Context = #ctx_ty;

          #[inline(always)]
          #[allow(unused_variables)]
          fn write<W: binmarshal::rw::BitWriter>(&self, writer: &mut W, ctx: #ctx_ty) -> bool {
            #(#to_unpack)*
            #magic_write && #to_write
          }

          #[inline(always)]
          #[allow(unused_variables)]
          fn read(view: &mut binmarshal::rw::BitView<'_>, ctx: #ctx_ty) -> Option<Self> {
            #magic_read
            #(#to_read)*
            Some(Self { #(#to_construct),* })
          }

          #[inline(always)]
          #[allow(unused_variables)]
          fn update(&mut self, ctx: &mut #ctx_ty) {
            #(#to_unpack_mutable)*
            #(#to_update);*
          }
        }
      };

      out.into()
    },
    syn::Data::Enum(en) => {
      let (write_tag, read_tag, update_tag) = match attrs.tag {
        Some(tag) => {
          let in_tag: TokenStream = parse_str(&tag).unwrap();
          (quote! { true }, quote! { let _tag = #in_tag; }, Some(in_tag))
        },
        None => {
          let tag_type = attrs.tag_type.clone().unwrap();

          let ctx_val = match attrs.tag_bits {
            Some(bits) => quote!{ binmarshal::BitSpecification::<#bits> {} },
            None => quote! { () }
          };
          
          (quote! {
            <#tag_type as binmarshal::BinMarshal<_>>::write(&_tag, writer, #ctx_val)
          },
          quote! {
            let _tag = <#tag_type as binmarshal::BinMarshal<_>>::read(view, #ctx_val)?;
          },
          None)
        }
      };

      let it = en.variants.into_iter().map(|variant| {
        let receiver = EnumVariantReceiver::from_variant(&variant).unwrap();
        let name = receiver.ident;
        let tag: TokenStream = parse_str(&receiver.tag).unwrap();

        let inner_update = update_tag.clone().map(|x| quote!{ #x = #tag }).unwrap_or(quote!{ });

        let r = match variant.fields {
          Fields::Named(named) => {
            let processed_fields = named.named.into_iter().enumerate().map(|(i, field)| process_struct_field(i, field));

            let write = processed_fields.clone().map(|t| t.0).reduce(|a, b| quote!{ #a && #b });
            let read = processed_fields.clone().map(|t| t.1);
            let construct = processed_fields.clone().map(|t| t.2);
            let destruct = processed_fields.clone().map(|t| t.3);
            let update = processed_fields.clone().map(|t| t.6).filter_map(|x| x);
            let construct2 = construct.clone();

            let read = quote!{
              (#tag) => {
                #(#read)*
                Some(Self::#name { #(#construct),* })
              }
            };

            let write_tag = quote!{
              Self::#name { .. } => #tag
            };

            let write = quote!{
              Self::#name { #(#construct2),* } => {
                #write
              }
            };

            let update = quote!{
              Self::#name { #(#destruct),* } => {
                #inner_update;
                #(#update);*
              }
            };

            ( read, write_tag, write, update )
          },
          Fields::Unnamed(unnamed) => {
            let processed_fields = unnamed.unnamed.into_iter().enumerate().map(|(i, field)| process_struct_field(i, field));

            let write = processed_fields.clone().map(|t| t.0).reduce(|a, b| quote!{ #a && #b });
            let read = processed_fields.clone().map(|t| t.1);
            let construct = processed_fields.clone().map(|t| t.2);
            let update = processed_fields.clone().map(|t| t.6).filter_map(|x| x);
            let construct2 = construct.clone();
            let construct3 = construct.clone();

            let read = quote!{
              (#tag) => {
                #(#read)*
                Some(Self::#name(#(#construct),*))
              }
            };

            let write_tag = quote!{
              Self::#name(..) => #tag
            };

            let write = quote!{
              Self::#name(#(#construct2),*) => {
                #write
              }
            };

            let update = quote!{
              Self::#name(#(#construct3),*) => {
                #inner_update;
                #(#update);*
              }
            };

            ( read, write_tag, write, update )
          },
          Fields::Unit => {
            ( quote! { (#tag) => Some(Self::#name) }, quote!{ Self::#name => #tag }, quote! { Self::#name => { true } }, quote! { Self::#name => { #inner_update } })
          },
        };

        ( r.0, r.1, r.2, r.3, quote!{ #name }, quote!{ Self::#name => #tag })
      });

      let variants_name = syn::Ident::new(&format!("{}Tags", ident), ident.span());

      let read_match_variants = it.clone().map(|v| v.0);
      let write_tag_match_variants = it.clone().map(|v| v.1);
      let write_match_variants = it.clone().map(|v| v.2);
      let update_variants = it.clone().map(|v| v.3);
      let variant_variants = it.clone().map(|v| v.4);
      let variant_body = it.clone().map(|v| v.5);

      let gwb = generics_without_bounds.clone();
      let tt = attrs.tag_type.unwrap_or(Path::from_string("u8").unwrap());

      let out = quote! {
        #magic_definition

        pub enum #variants_name {
          #(#variant_variants),*
        }

        impl #variants_name {
          pub fn to_tag(&self) -> #tt {
            match self {
              #(#variant_body),*
            }
          }
        }

        impl #generics binmarshal::HasTags for #ident<#(#gwb),*> {
          type Tags = #variants_name;
        }

        impl #generics binmarshal::BinMarshal<#ctx_ty> for #ident<#(#generics_without_bounds),*> {
          type Context = #ctx_ty;

          #[inline(always)]
          #[allow(unused_variables)]
          fn write<W: binmarshal::rw::BitWriter>(&self, writer: &mut W, ctx: #ctx_ty) -> bool {
            let _tag = match &self {
              #(#write_tag_match_variants),*
            };
            #magic_write && #write_tag && match self {
              #(#write_match_variants),*
            }
          }

          #[inline(always)]
          #[allow(unused_variables)]
          fn read(view: &mut binmarshal::rw::BitView<'_>, ctx: #ctx_ty) -> Option<Self> {
            #magic_read
            #read_tag
            match _tag {
              #(#read_match_variants),*,
              _ => None
            }
          }

          #[inline(always)]
          #[allow(unused_variables)]
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

struct Magic {
  name: Ident,
  magic: syn::LitByteStr
}

impl Parse for Magic {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let name = input.parse()?;
    input.parse::<Token![,]>()?;
    let magic = input.parse()?;

    Ok(Self { name, magic })
  }
}

fn gen_magic(m: Magic) -> proc_macro::TokenStream {
  let Magic { name, magic } = m;
  
  quote! {
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct #name;

    impl binmarshal::BinMarshal<()> for #name {
      type Context = ();

      fn write<W: binmarshal::rw::BitWriter>(&self, writer: &mut W, _ctx: ()) -> bool {
        #magic.write(writer, _ctx)
      }

      fn read(view: &mut binmarshal::rw::BitView<'_>, _ctx: ()) -> Option<Self> {
        if Some(#magic) == binmarshal::BinMarshal::<()>::read(view, _ctx).as_ref() {
          Some(Self)
        } else {
          None
        }
      }

      fn update(&mut self, _ctx: &mut ()) { }
    }
  }.into()
}

#[proc_macro]
pub fn magic(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
  gen_magic(parse_macro_input!(item as Magic))
}