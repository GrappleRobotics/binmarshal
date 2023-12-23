use darling::{FromField, FromAttributes, FromVariant};
use proc_macro2::{TokenStream, Span, Literal};
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

#[derive(Debug, FromField)]
#[darling(attributes(marshal))]
struct StructFieldReceiver {
  ident: Option<Ident>,
  ty: Type,

  align: Option<usize>,
  bits: Option<usize>,
  ctx: Option<String>,
  forward_ctx: Option<bool>,
  asymmetric_ctx: Option<bool>,
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

  let ctx_val = if let Some(bits) = receiver.bits {
    quote! { binmarshal::BitSpecification::<#bits> {} }
  } else if let Some(ctx) = receiver.ctx.clone() {
    let parsed: TokenStream = parse_str(&ctx).unwrap();
    quote! { binmarshal::SelfType::<<#ty as binmarshal::BinMarshal<_>>::Context> #parsed }
  } else if Some(true) == receiver.forward_ctx {
    quote! { ctx }
  } else if Some(true) == receiver.asymmetric_ctx {
    quote! { ctx.into() }
  } else {
    quote! { () }
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
    let #var_name = self.#accessor;
  };

  let unpack_mutable = quote! {
    let #var_name = &mut self.#accessor;
  };

  let construct = quote! { #var_name };
  let destruct = quote!{ #var_name };

  let update = match (receiver.ctx, receiver.asymmetric_ctx, receiver.forward_ctx) {
    (_, _, Some(true)) => {
      // Forward context directly
      Some(quote! { #var_name.update(ctx) })
    }
    (_, Some(true), _) => {
      // Asymmetric context
      Some(quote! {
        let mut tmp_ctx = ctx.into();
        #var_name.update(&mut tmp_ctx);
        // *ctx = tmp_ctx.into();
      })
      // let parsed: TokenStream = parse_str(&ctx).unwrap();
      // Some(quote! { #var_name_ref.update(binmarshal::SelfType::<<<#ty as binmarshal::BinMarshal<_>>::Context as binmarshal::BinmarshalContext>::MutableComplement<'_>> #parsed) })
    },
    (Some(ctx), _, _) => {
      let parsed: TokenStream = parse_str(&ctx).unwrap();
      Some(quote! { #var_name.update(binmarshal::SelfType::<<<#ty as binmarshal::BinMarshal<_>>::Context as binmarshal::BinmarshalContext>::MutableComplement<'_>> #parsed) })
    },
    _ => None,
  };

  (write, read, construct, destruct, unpack, unpack_mutable, update)
}

#[proc_macro_derive(BinMarshal, attributes(marshal))]
pub fn derive_bin_marshal(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let DeriveInput {
    attrs, vis: _, ident, generics: _, data
  } = parse_macro_input!(input as DeriveInput);

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

        impl binmarshal::BinMarshal<#ctx_ty> for #ident {
          type Context = #ctx_ty;

          #[inline(always)]
          #[allow(unused_variables)]
          fn write<W: binmarshal::rw::BitWriter>(self, writer: &mut W, ctx: #ctx_ty) -> bool {
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
          fn update<'a>(&'a mut self, ctx: <#ctx_ty as binmarshal::BinmarshalContext>::MutableComplement<'a>) {
            #(#to_unpack_mutable)*
            #(#to_update);*
          }
        }
      };

      println!("{}", out.to_string());

      out.into()
    },
    syn::Data::Enum(en) => {
      let (write_tag, read_tag, update_tag) = match attrs.tag {
        Some(tag) => {
          let in_tag: TokenStream = parse_str(&tag).unwrap();
          (quote! { true }, quote! { let _tag = #in_tag; }, Some(in_tag))
        },
        None => {
          let tag_type = attrs.tag_type.unwrap();

          let ctx_val = match attrs.tag_bits {
            Some(bits) => quote!{ binmarshal::BitSpecification::<#bits> {} },
            None => quote! { () }
          };
          
          (quote! {
            <#tag_type as binmarshal::BinMarshal<_>>::write(_tag, writer, #ctx_val)
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

        let inner_update = update_tag.clone().map(|x| quote!{ *#x = #tag }).unwrap_or(quote!{ });

        match variant.fields {
          Fields::Named(named) => {
            let processed_fields = named.named.into_iter().enumerate().map(|(i, field)| process_struct_field(i, field));

            let write = processed_fields.clone().map(|t| t.0).reduce(|a, b| quote!{ #a && #b });
            let read = processed_fields.clone().map(|t| t.1);
            let construct = processed_fields.clone().map(|t| t.2);
            let destruct = processed_fields.clone().map(|t| t.3);
            let update = processed_fields.clone().map(|t| t.6).filter_map(|x| x);
            let construct2 = construct.clone();
            let construct3 = construct.clone();

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
        }
      });

      let read_match_variants = it.clone().map(|(read, _, _, _)| read);
      let write_tag_match_variants = it.clone().map(|(_, write_tag, _, _)| write_tag);
      let write_match_variants = it.clone().map(|(_, _, write, _)| write);
      let update_variants = it.clone().map(|(_, _, _, update)| update);

      let out = quote! {
        #magic_definition

        impl binmarshal::BinMarshal<#ctx_ty> for #ident {
          type Context = #ctx_ty;

          #[inline(always)]
          #[allow(unused_variables)]
          fn write<W: binmarshal::rw::BitWriter>(self, writer: &mut W, ctx: #ctx_ty) -> bool {
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
          fn update<'a>(&'a mut self, ctx: <#ctx_ty as binmarshal::BinmarshalContext>::MutableComplement<'a>) {
            match self {
              #(#update_variants),*
            }
          }
        }
      };

      println!("{}", out.to_string());

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

// #[proc_macro_derive(Context)]
// pub fn derive_context(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
//   let DeriveInput {
//     attrs: _, vis, ident, generics: _, data
//   } = parse_macro_input!(input as DeriveInput);

//   let new_ident = syn::Ident::new(&format!("{}Complement", ident), ident.span());
//   match data {
//     syn::Data::Struct(st) => {
//       let out = match st.fields {
//         Fields::Named(named) => {
//           let fields = named.named.into_iter().map(|field| {
//             let Field { attrs, vis, mutability: _, ident, colon_token, ty } = field;
//             quote! { #(#attrs)* #vis #ident #colon_token &'a mut #ty }
//           });

//           quote! {
//             #vis struct #new_ident<'a> {
//               #(#fields,)*
//             }

//             impl binmarshal::BinmarshalContext for #ident {
//               type MutableComplement<'a> = #new_ident<'a>;
//             }
//           }.into()
//         },
//         Fields::Unnamed(unnamed) => {
//           let fields = unnamed.unnamed.into_iter().map(|field| {
//             let Field { attrs, vis, mutability: _, ident: _, colon_token: _, ty } = field;
//             quote! { #(#attrs)* #vis &'a mut #ty }
//           });

//           quote! {
//             #vis struct #new_ident<'a>(#(#fields,)*);

//             impl binmarshal::BinmarshalContext for #ident {
//               type MutableComplement<'a> = #new_ident<'a>;
//             }
//           }.into()
//         },
//         Fields::Unit => panic!("Context structs must have data!"),
//       };

//       out
//     },
//     syn::Data::Enum(_) => panic!("Context cannot be derived for an Enum - it must be a struct"),
//     syn::Data::Union(_) => panic!("Context cannot be derived for a Union - it must be a struct"),
//   }
// }

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

      fn write<W: binmarshal::rw::BitWriter>(self, writer: &mut W, _ctx: ()) -> bool {
        #magic.to_owned().write(writer, _ctx)
      }

      fn read(view: &mut binmarshal::rw::BitView<'_>, _ctx: ()) -> Option<Self> {
        if Some(#magic.to_owned()) == binmarshal::BinMarshal::<()>::read(view, _ctx) {
          Some(Self)
        } else {
          None
        }
      }

      fn update<'a>(&'a mut self, _ctx: <() as binmarshal::BinmarshalContext>::MutableComplement<'a>) { }
    }
  }.into()
}

#[proc_macro]
pub fn magic(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
  gen_magic(parse_macro_input!(item as Magic))
}