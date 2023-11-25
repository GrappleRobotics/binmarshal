use darling::{FromField, FromAttributes, FromVariant};
use proc_macro2::{TokenStream, Span};
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Type, Ident, parse_str, Path, Fields, Field};

#[derive(Debug, FromAttributes)]
#[darling(attributes(marshal))]
struct StructOrEnumReceiver {
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
  ctx: Option<String>
}

#[derive(Debug, FromVariant)]
#[darling(attributes(marshal))]
struct EnumVariantReceiver {
  ident: Ident,
  
  tag: String
}

fn process_struct_field(i: usize, field: Field) -> (TokenStream, TokenStream, TokenStream, TokenStream) {
  let idx = syn::Index::from(i);

  let receiver = StructFieldReceiver::from_field(&field).unwrap();

  let (accessor, var_name) = match receiver.ident {
    Some(ident) => (quote! { #ident }, ident),
    None => (quote! { #idx }, syn::Ident::new(format!("_{}", i).as_str(), Span::call_site()))
  };

  let ty = receiver.ty;

  let ctx_val = if let Some(bits) = receiver.bits {
    quote! { BitSpecification::<#bits> {} }
  } else if let Some(ctx) = receiver.ctx {
    parse_str(&ctx).unwrap()
  } else {
    quote! { () }
  };

  let (align_write, align_read) = receiver.align.map(|align| (quote!{ writer.align(#align) }, quote!{ view.align(#align) })).unwrap_or((quote!{ true }, quote!{ true }));

  let write = quote! {
    (#align_write) && (<#ty as BinMarshal<_>>::write(#var_name, writer, #ctx_val))
  };

  let read = quote! {
    #align_read;
    let #var_name = <#ty as BinMarshal<_>>::read(view, #ctx_val)?;
  };

  let unpack = quote! {
    let #var_name = self.#accessor;
  };

  let construct = quote! { #var_name };

  (write, read, construct, unpack)
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

  match data {
    syn::Data::Struct(st) => {
      let it = st.fields.into_iter().enumerate().map(|(i, field)| process_struct_field(i, field));

      let to_write = it.clone().map(|x| x.0).reduce(|a, b| quote!{ #a && #b });
      let to_read = it.clone().map(|x| x.1);
      let to_construct = it.clone().map(|x| x.2);
      let to_unpack = it.clone().map(|x| x.3);
      
      let out = quote! {
        impl BinMarshal<#ctx_ty> for #ident {
          #[inline(always)]
          #[allow(unused_variables)]
          fn write<W: BitWriter>(self, writer: &mut W, ctx: #ctx_ty) -> bool {
            #(#to_unpack)*
            #to_write
          }

          #[inline(always)]
          #[allow(unused_variables)]
          fn read(view: &mut BitView<'_>, ctx: #ctx_ty) -> Option<Self> {
            #(#to_read)*
            Some(Self { #(#to_construct),* })
          }
        }
      };

      out.into()
    },
    syn::Data::Enum(en) => {
      let tag_type = attrs.tag_type.unwrap();

      let (write_tag, read_tag) = match attrs.tag {
        Some(tag) => {
          let in_tag: TokenStream = parse_str(&tag).unwrap();
          (quote! { true }, quote! { let _tag = #in_tag; })
        },
        None => {
          let ctx_val = match attrs.tag_bits {
            Some(bits) => quote!{ BitSpecification::<#bits> {} },
            None => quote! { () }
          };
          
          (quote! {
            <#tag_type as BinMarshal<_>>::write(_tag, writer, #ctx_val)
          },
          quote! {
            let _tag = <#tag_type as BinMarshal<_>>::read(view, #ctx_val)?;
          })
        }
      };

      let it = en.variants.into_iter().map(|variant| {
        let receiver = EnumVariantReceiver::from_variant(&variant).unwrap();
        let name = receiver.ident;
        let tag: TokenStream = parse_str(&receiver.tag).unwrap();

        match variant.fields {
          Fields::Named(named) => {
            let processed_fields = named.named.into_iter().enumerate().map(|(i, field)| process_struct_field(i, field));

            let write = processed_fields.clone().map(|t| t.0).reduce(|a, b| quote!{ #a && #b });
            let read = processed_fields.clone().map(|t| t.1);
            let construct = processed_fields.clone().map(|t| t.2);
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

            ( read, write_tag, write )
          },
          Fields::Unnamed(unnamed) => {
            let processed_fields = unnamed.unnamed.into_iter().enumerate().map(|(i, field)| process_struct_field(i, field));

            let write = processed_fields.clone().map(|t| t.0).reduce(|a, b| quote!{ #a && #b });
            let read = processed_fields.clone().map(|t| t.1);
            let construct = processed_fields.clone().map(|t| t.2);
            let construct2 = construct.clone();

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

            ( read, write_tag, write )
          },
          Fields::Unit => {
            ( quote! { (#tag) => Some(Self::#name) }, quote!{ Self::#name => #tag }, quote! { Self::#name => { true } })
          },
        }
      });

      let read_match_variants = it.clone().map(|(read, _, _)| read);
      let write_tag_match_variants = it.clone().map(|(_, write_tag, _)| write_tag);
      let write_match_variants = it.clone().map(|(_, _, write)| write);

      let out = quote! {
        impl BinMarshal<#ctx_ty> for #ident {
          #[inline(always)]
          #[allow(unused_variables)]
          fn write<W: BitWriter>(self, writer: &mut W, ctx: #ctx_ty) -> bool {
            let _tag = match &self {
              #(#write_tag_match_variants),*
            };
            #write_tag && match self {
              #(#write_match_variants),*
            }
          }

          #[inline(always)]
          #[allow(unused_variables)]
          fn read(view: &mut BitView<'_>, ctx: #ctx_ty) -> Option<Self> {
            #read_tag
            match _tag {
              #(#read_match_variants),*,
              _ => None
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

  match data {
    syn::Data::Struct(st) => {
      match st.fields {
        Fields::Unnamed(fields) => {
          let field = &fields.unnamed[0];
          let mut extra_clone = vec![];

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

          let out = quote! {
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

          out.into()
        },
        _ => panic!("Proxy only supported on newtype structs"),
      }
    },
    syn::Data::Enum(_) => panic!("Proxy not supported on Enum types"),
    syn::Data::Union(_) => panic!("Proxy not supported on Union types"),
  }
}