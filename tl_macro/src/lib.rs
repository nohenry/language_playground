#![feature(iter_intersperse)]
use std::{collections::HashSet, str::FromStr};

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream, Parser},
    parse_macro_input,
    punctuated::Punctuated,
    Attribute, Data, DeriveInput, Fields, FieldsNamed, FieldsUnnamed, GenericArgument,
    GenericParam, ReturnType, Type,
};

fn has_attr<'a>(attrs: &'a Vec<Attribute>, st: &[&str]) -> Option<&'a Attribute> {
    attrs.iter().find(|a| {
        let segs = &a.meta.path().segments;
        if segs.len() != st.len() {
            return false;
        }

        for (a, b) in segs.iter().zip(st.iter()) {
            if a.ident.to_string() != *b {
                return false;
            }
        }

        true
    })
}

fn has_many_attr<'a>(
    attrs: &'a Vec<Attribute>,
    st: &'a [&str],
) -> impl Iterator<Item = &'a Attribute> + 'a {
    attrs.iter().filter(|a| {
        let segs = &a.meta.path().segments;
        if segs.len() != st.len() {
            return false;
        }

        for (a, b) in segs.iter().zip(st.iter()) {
            if a.ident.to_string() != *b {
                return false;
            }
        }

        true
    })
    // .cloned()
    // .collect()
}

#[derive(Debug)]
enum ArgType {
    Ident(String),
    Type(Type),
}

impl Parse for ArgType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(syn::Token![type]) {
            let _type: syn::Token![type] = input.parse()?;
            let _eq: syn::Token![=] = input.parse()?;
            Ok(ArgType::Type(input.parse()?))
        } else {
            let ident: syn::Ident = input.parse()?;
            Ok(ArgType::Ident(ident.to_string()))
        }
    }
}

fn attr_args(attr: &Attribute) -> (HashSet<String>, HashSet<Type>) {
    match &attr.meta {
        syn::Meta::List(list) => {
            let parser = Punctuated::<ArgType, syn::Token![,]>::parse_separated_nonempty;
            let args = parser.parse(list.tokens.clone().into()).unwrap();

            let all: (HashSet<_>, HashSet<_>) = args
                .into_iter()
                .map(|f| match f {
                    ArgType::Ident(ident) => (Some(ident), None),
                    ArgType::Type(ty) => (None, Some(ty)),
                })
                .unzip();

            (
                all.0.into_iter().flatten().collect(),
                all.1.into_iter().flatten().collect(),
            )
        }
        _ => (HashSet::new(), HashSet::new()),
    }
}

fn get_write_format(attr: &Attribute) -> Option<&TokenStream> {
    match &attr.meta {
        syn::Meta::List(list) => Some(&list.tokens),
        _ => None,
    }
}

fn is_optional(ty: &Type) -> bool {
    match &ty {
        syn::Type::Path(path) => {
            let segs = &path.path.segments;
            if segs.len() != 1 {
                return false;
            }
            segs.first().unwrap().ident.to_string() == "Option"
        }
        _ => false,
    }
}

fn is_box(ty: &Type) -> bool {
    match &ty {
        syn::Type::Path(path) => {
            let segs = &path.path.segments;
            if segs.len() != 1 {
                return false;
            }
            segs.first().unwrap().ident.to_string() == "Box"
        }
        _ => false,
    }
}

fn is_optional_box(ty: &Type) -> bool {
    match &ty {
        syn::Type::Path(path) => {
            let segs = &path.path.segments;
            if segs.len() != 1 {
                return false;
            }

            let seg = segs.first().unwrap();
            if seg.ident.to_string() != "Option" {
                return false;
            }

            let syn::PathArguments::AngleBracketed(angs) = &seg.arguments else {
                return false;
            };

            let Some(GenericArgument::Type(arg)) = angs.args.first() else {
                return false;
            };

            is_box(arg)
        }
        _ => false,
    }
}

fn types_match(pattern: &Type, source: &Type) -> bool {
    match (pattern, source) {
        (Type::Infer(_), _) => true,
        (a, b) if a == b => {
            return true;
        }
        (Type::Tuple(tuple), Type::Tuple(source_tuple)) => {
            if tuple.elems.len() != source_tuple.elems.len() {
                return false;
            }

            for (elem, source_elem) in tuple.elems.iter().zip(source_tuple.elems.iter()) {
                if !types_match(elem, source_elem) {
                    return false;
                }
            }

            true
        }
        (Type::Path(path), Type::Path(source_path)) => {
            let path_segs = &path.path.segments;
            let source_path_segs = &source_path.path.segments;

            if path_segs.len() != source_path_segs.len() {
                return false;
            }

            for (path, source_path) in path_segs.iter().zip(source_path_segs.iter()) {
                if path.ident != source_path.ident {
                    return false;
                }

                match (&path.arguments, &source_path.arguments) {
                    (a, b) if a == b => return true,
                    (
                        syn::PathArguments::AngleBracketed(angs),
                        syn::PathArguments::AngleBracketed(source_angs),
                    ) => {
                        if angs.args.len() != source_angs.args.len() {
                            return false;
                        }

                        for (ang, source_ang) in angs.args.iter().zip(source_angs.args.iter()) {
                            match (ang, source_ang) {
                                (GenericArgument::Type(ty), GenericArgument::Type(source_ty)) => {
                                    if !types_match(ty, source_ty) {
                                        return false;
                                    }
                                }
                                (ang, source_ang) if ang != source_ang => return false,
                                _ => (),
                            }
                        }
                    }
                    (
                        syn::PathArguments::Parenthesized(angs),
                        syn::PathArguments::Parenthesized(source_angs),
                    ) => {
                        if angs.inputs.len() != source_angs.inputs.len() {
                            return false;
                        }

                        for (ty, source_ty) in angs.inputs.iter().zip(source_angs.inputs.iter()) {
                            if !types_match(ty, source_ty) {
                                return false;
                            }
                        }

                        match (&angs.output, &source_angs.output) {
                            (ReturnType::Type(_, ty), ReturnType::Type(_, source_ty)) => {
                                if !types_match(&ty, &source_ty) {
                                    return false;
                                }
                            }
                            (a, b) if a != b => return false,
                            _ => (),
                        }
                    }
                    (_, _) => return false,
                }
            }

            true
        }
        _ => false,
    }
}

#[cfg(test)]
mod test {
    use std::str::FromStr;

    use proc_macro::TokenStream;
    use syn::Type;

    use crate::types_match;

    #[test]
    fn test_type_match() {
        let p = syn::parse::<Type>(TokenStream::from_str("_").unwrap()).unwrap();
        let s = syn::parse::<Type>(TokenStream::from_str("i32").unwrap()).unwrap();

        println!("{}", types_match(&p, &s));
    }
}

#[proc_macro_derive(
    NodeFormat,
    attributes(transient, ignore_item, ignore_all, keep_item, extra_format)
)]
pub fn derive_node_format(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let ident = input.ident;

    let params = input.generics.params;
    let where_clause = input.generics.where_clause;

    let struct_args = params.iter().map(|param| match param {
        GenericParam::Type(tp) => tp.ident.to_token_stream(),
        GenericParam::Const(tp) => tp.ident.to_token_stream(),
        GenericParam::Lifetime(tp) => tp.lifetime.to_token_stream(),
    });

    let tokens = match &input.data {
        Data::Enum(node) => {
            let mut tokens = TokenStream::new();

            let (ignore_all, _) = has_attr(&input.attrs, &["ignore_all"])
                .map(|attr| attr_args(attr))
                .unwrap_or((HashSet::new(), HashSet::new()));

            for variant in &node.variants {
                let name = &variant.ident;

                if ignore_all.contains(&name.to_string()) {
                    continue;
                }

                if has_attr(&variant.attrs, &["ignore_item"]).is_some() {
                    continue;
                }

                let extras = has_many_attr(&variant.attrs, &["extra_format"])
                    .map(|attr| get_write_format(attr).unwrap());

                let mut globber = TokenStream::new();

                let fields_in_variant = match &variant.fields {
                    Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                        let fields = unnamed
                            .iter()
                            .enumerate()
                            .map(|f| TokenStream::from_str(&format!("a{}", f.0)).unwrap());

                        let glob_fields = fields.clone();
                        globber.extend(quote::quote! { let this = (#(#glob_fields),*); });

                        quote::quote! { (#(#fields),*) }
                    }
                    Fields::Unit => quote::quote! {},
                    Fields::Named(FieldsNamed { named, .. }) => {
                        let fields = named.iter().map(|f| &f.ident);

                        quote::quote! { {#(#fields),*} }
                    }
                };

                let string = name.to_string();

                tokens.extend(quote::quote! {
                    #ident::#name #fields_in_variant => { #globber write!(f, #string)?; #(write!(f, #extras)?;)* Ok(()) },
                })
            }

            quote::quote! {
                impl <#params> NodeDisplay for #ident<#(#struct_args),*> #where_clause {
                    #![allow(dead_code)]
                    #![allow(unused_variables)]

                    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
                        match self {
                            #tokens
                            _ => Ok(())
                        }
                    }
                }
            }
        }
        Data::Struct(_node) => {
            let extras = has_many_attr(&input.attrs, &["extra_format"])
                .map(|attr| get_write_format(attr).unwrap());

            let name = ident.to_string();

            quote::quote! {
                impl <#params> NodeDisplay for #ident<#(#struct_args),*> #where_clause {
                    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
                        write!(f, #name)?;
                        #(write!(f, #extras)?;)*
                        Ok(())
                    }
                }
            }
        }
        _ => panic!("Unexpected type"),
    };

    tokens.into_token_stream().into()
}

fn derive_tree_format_helper(
    ident: &syn::Ident,
    fields: &Fields,
    enum_field: Option<&syn::Ident>,
    (ignore_items, ignore_types): &(HashSet<String>, HashSet<Type>),
) -> (TokenStream, TokenStream) {
    let mut num_childs = TokenStream::new();
    let mut childs = TokenStream::new();

    let enum_field = enum_field
        .map(|field| quote::quote!(::#field))
        .unwrap_or(quote::quote!());

    match fields {
        Fields::Unnamed(fields) => {
            let mut num_child_values = quote::quote!(0);
            let mut child_values = TokenStream::new();

            let names: String = fields
                .unnamed
                .iter()
                .enumerate()
                .map(|(i, _)| format!("_a{}", i))
                .intersperse(",".to_string())
                .collect();
            let name_toks = TokenStream::from_str(&names).unwrap();

            for (i, field) in fields.unnamed.iter().enumerate() {
                let field_name = TokenStream::from_str(&format!("_a{}", i)).unwrap();

                if ignore_items.contains(&field_name.to_string()) {
                    continue;
                }

                if ignore_types.contains(&field.ty)
                    && has_attr(&field.attrs, &["keep_item"]).is_none()
                {
                    continue;
                }

                if has_attr(&field.attrs, &["ignore_item"]).is_some() {
                    continue;
                }

                // if has_attr(&field.attrs, &["transient"]) {
                //     num_child_values.extend(quote::quote! {
                //         + #field_name.len()
                //     });
                // } else
                if is_optional_box(&field.ty) {
                    num_child_values.extend(quote::quote! {
                        + addup!(#field_name)
                    });

                    child_values.extend(quote::quote!(#field_name.map_tree(),));
                } else if is_box(&field.ty) {
                    num_child_values.extend(quote::quote! {
                        + 1
                    });

                    child_values.extend(quote::quote!(Some(&**#field_name),));
                } else if is_optional(&field.ty) {
                    num_child_values.extend(quote::quote! {
                        + addup!(#field_name)
                    });

                    child_values.extend(quote::quote!(#field_name,));
                } else {
                    num_child_values.extend(quote::quote! {
                        + 1
                    });

                    child_values.extend(quote::quote!(Some(#field_name),));
                }
            }

            num_childs.extend(quote::quote! {
                #ident #enum_field (#name_toks) => {
                    #num_child_values
                }
            });

            childs.extend(quote::quote! {
                #ident #enum_field (#name_toks) => {
                    switchon!(index, #child_values);
                    None
                }
            });
        }
        Fields::Unit => {
            num_childs.extend(quote::quote! {
                #ident #enum_field  => 0,
            });

            childs.extend(quote::quote! {
                #ident #enum_field => None,
            });
        }
        Fields::Named(fields) => {
            let mut num_child_values = quote::quote!(0);
            let mut child_values = TokenStream::new();

            let names: String = fields
                .named
                .iter()
                .map(|field| field.ident.as_ref().unwrap().to_string())
                .intersperse(",".to_string())
                .collect();
            let name_toks = TokenStream::from_str(&names).unwrap();

            for field in fields.named.iter() {
                let field_name = field.ident.as_ref().unwrap();

                if ignore_items.contains(&field_name.to_string()) {
                    continue;
                }

                if ignore_types.contains(&field.ty)
                    && has_attr(&field.attrs, &["keep_item"]).is_none()
                {
                    continue;
                }

                if has_attr(&field.attrs, &["ignore_item"]).is_some() {
                    continue;
                }

                // if has_attr(&field.attrs, &["transient"]) {
                //     num_child_values.extend(quote::quote! {
                //         + #field_name.len()
                //     });
                // } else
                if is_optional_box(&field.ty) {
                    num_child_values.extend(quote::quote! {
                        + addup!(#field_name)
                    });

                    child_values.extend(quote::quote!(#field_name.map_tree(),));
                } else if is_box(&field.ty) {
                    num_child_values.extend(quote::quote! {
                        + 1
                    });

                    child_values.extend(quote::quote!(Some(&**#field_name),));
                } else if is_optional(&field.ty) {
                    num_child_values.extend(quote::quote! {
                        + addup!(#field_name)
                    });

                    child_values.extend(quote::quote!(#field_name,));
                } else {
                    num_child_values.extend(quote::quote! {
                        + 1
                    });

                    child_values.extend(quote::quote!(Some(#field_name),));
                }
            }

            num_childs.extend(quote::quote! {
                #ident #enum_field {#name_toks} => {
                    #num_child_values
                }
            });

            childs.extend(quote::quote! {
                #ident #enum_field {#name_toks} => {
                    switchon!(index, #child_values);
                    None
                }
            });
        }
    };

    (num_childs, childs)
}

#[proc_macro_derive(
    TreeFormat,
    attributes(transient, ignore_item, ignore_all, keep_item, semantic)
)]
pub fn derive_tree_format(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let ident = input.ident;

    let ignore = has_attr(&input.attrs, &["ignore_all"])
        .map(|attr| attr_args(attr))
        .unwrap_or((HashSet::new(), HashSet::new()));

    let params = input.generics.params;
    let where_clause = input.generics.where_clause;

    let struct_args = params.iter().map(|param| match param {
        GenericParam::Type(tp) => tp.ident.to_token_stream(),
        GenericParam::Const(tp) => tp.ident.to_token_stream(),
        GenericParam::Lifetime(tp) => tp.lifetime.to_token_stream(),
    });

    let mut num_childs = TokenStream::new();
    let mut childs = TokenStream::new();
    let mut semantic_types = TokenStream::new();
    let boxed_childs = TokenStream::new();

    let default = if let Some(attr) = has_attr(&input.attrs, &["semantic"]) {
        let tokens = attr.meta.require_list().unwrap().tokens.clone();

        quote::quote! { _ => #tokens, }
    } else {
        quote::quote! { _ => SemanticType::Default }
    };

    match &input.data {
        Data::Enum(node) => {
            for variant in &node.variants {
                if ignore.0.contains(&variant.ident.to_string()) {
                    continue;
                }

                if has_attr(&variant.attrs, &["ignore_item"]).is_some() {
                    continue;
                }

                if let Some(attr) = has_attr(&variant.attrs, &["semantic"]) {
                    let name = &variant.ident;
                    let tokens = attr.meta.require_list().unwrap().tokens.clone();

                    semantic_types.extend(quote::quote! { #ident::#name { .. } => #tokens, });
                }

                let (num_child, child) = derive_tree_format_helper(
                    &ident,
                    &variant.fields,
                    Some(&variant.ident),
                    &ignore,
                );

                num_childs.extend(num_child);
                childs.extend(child);
            }
        }
        Data::Struct(node) => {
            let (num_child, child) = derive_tree_format_helper(&ident, &node.fields, None, &ignore);

            num_childs.extend(num_child);
            childs.extend(child);
        }
        _ => panic!("Unexpected type"),
    };

    let tokens = quote::quote! {
        impl <#params> TreeDisplay for #ident<#(#struct_args),*> #where_clause {

            fn semantic_type(&self) -> SemanticType {
                match self {
                    #semantic_types
                    #default
                }
            }

            fn num_children(&self, _cfg: &Config) -> usize {
                match self {
                    #num_childs
                    _ => 0
                }
            }

            fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
                match self {
                    #childs
                    _ => None
                }
            }

            fn child_at_bx<'b>(&'b self, index: usize, _cfg: &Config) -> Box<dyn TreeDisplay + 'b> {
                match self {
                    #boxed_childs
                    _ => panic!("Unexpected index for enum!")
                }
            }
        }
    };

    tokens.into()
}

fn derive_ast_node_helper(
    ident: &syn::Ident,
    fields: &Fields,
    enum_field: Option<&syn::Ident>,
    (ignore_items, ignore_types): &(HashSet<String>, HashSet<Type>),
) -> TokenStream {
    let mut output = TokenStream::new();
    match &fields {
        Fields::Unnamed(fields) => {
            let mut generate = |left: i32,
                                mut right: i32,
                                left_name: &TokenStream,
                                stream: &TokenStream| {
                while left <= right && right >= 0 {
                    let right_field = &fields.unnamed[right as usize];

                    if has_attr(&right_field.attrs, &["skip_item"]).is_some()
                        || ignore_types.contains(&right_field.ty)
                    {
                        right -= 1;
                        continue;
                    }

                    let right_str = format!("_a{}", right);
                    let right_name = TokenStream::from_str(&right_str).unwrap();

                    if ignore_items.contains(&right_str) {
                        right -= 1;
                        continue;
                    }

                    let ignore_rights: String = (right as usize..fields.unnamed.len() - 1)
                        .map(|_| "_")
                        .intersperse(",")
                        .collect();

                    let mut out = stream.clone();

                    if left != right {
                        if is_optional(&fields.unnamed[right as usize].ty) {
                            out.extend(quote::quote! { Some(#right_name) });
                        } else {
                            out.extend(right_name.clone());
                        }
                    }

                    if ignore_rights.len() > 0 {
                        if left != right {
                            out.extend(quote::quote!(,));
                        }
                        out.extend(TokenStream::from_str(&ignore_rights));
                    }

                    if let Some(enum_field) = enum_field {
                        output.extend(quote::quote! {
                            #ident::#enum_field (#out) => Range::from((&#left_name.get_range(), &#right_name.get_range())),
                        });
                    } else {
                        output.extend(quote::quote! {
                            #ident (#out) => Range::from((&#left_name.get_range(), &#right_name.get_range())),
                        });
                    }

                    if is_optional(&fields.unnamed[right as usize].ty) {
                        right -= 1
                    } else {
                        break;
                    }
                }
            };

            let mut left = 0;
            let right = fields.unnamed.len() as i32 - 1;

            while left < fields.unnamed.len() as i32 {
                let left_field = &fields.unnamed[left as usize];

                if has_attr(&left_field.attrs, &["skip_item"]).is_some()
                    || ignore_types.contains(&left_field.ty)
                {
                    left += 1;
                    continue;
                }

                let left_str = format!("_a{}", left);
                let left_name = TokenStream::from_str(&left_str).unwrap();

                if ignore_items.contains(&left_str) {
                    left -= 1;
                    continue;
                }

                let mut inner_names = TokenStream::new();
                let ignore_lefts: String =
                    (0..left as usize).map(|_| "_").intersperse(",").collect();

                if ignore_lefts.len() > 0 {
                    inner_names.extend(TokenStream::from_str(&ignore_lefts));
                    inner_names.extend(quote::quote!(,));
                }

                if is_optional(&left_field.ty) {
                    inner_names.extend(quote::quote! { Some(#left_name) });
                } else {
                    inner_names.extend(left_name.clone());
                }

                inner_names.extend(quote::quote!(,..,));

                generate(left, right, &left_name, &inner_names);

                if is_optional(&left_field.ty) {
                    left += 1;
                } else {
                    break;
                }
            }
        }
        Fields::Unit => {
            if let Some(enum_field) = enum_field {
                output.extend(quote::quote! {
                    #ident::#enum_field => Range::default(),
                });
            } else {
                output.extend(quote::quote! {
                    #ident => Range::default(),
                });
            }
        }
        Fields::Named(fields) => {
            let mut generate = |left: i32,
                                mut right: i32,
                                left_name: &TokenStream,
                                stream: &TokenStream| {
                while left <= right && right >= 0 {
                    let right_field = &fields.named[right as usize];

                    if has_attr(&right_field.attrs, &["skip_item"]).is_some()
                        || ignore_types.contains(&right_field.ty)
                        || ignore_items.contains(&right_field.ident.as_ref().unwrap().to_string())
                    {
                        right -= 1;
                        continue;
                    }

                    let right_name = right_field.ident.clone().unwrap().to_token_stream();

                    let mut out = stream.clone();

                    if left != right {
                        if is_optional(&right_field.ty) {
                            out.extend(quote::quote! { #right_name: Some(#right_name) });
                        } else {
                            out.extend(right_name.clone());
                        }
                        out.extend(quote::quote! {,});
                    }

                    if let Some(enum_field) = enum_field {
                        output.extend(quote::quote! {
                                        #ident::#enum_field { #out .. } => Range::from((&#left_name.get_range(), &#right_name.get_range())),
                                    });
                    } else {
                        output.extend(quote::quote! {
                                        #ident { #out .. } => Range::from((&#left_name.get_range(), &#right_name.get_range())),
                                    });
                    }

                    if is_optional(&right_field.ty) {
                        right -= 1
                    } else {
                        break;
                    }
                }
            };

            let mut left = 0;
            let right = fields.named.len() as i32 - 1;

            while left < fields.named.len() as i32 {
                let left_field = &fields.named[left as usize];

                if has_attr(&left_field.attrs, &["skip_item"]).is_some()
                    || ignore_types.contains(&left_field.ty)
                    || ignore_items.contains(&left_field.ident.as_ref().unwrap().to_string())
                {
                    left += 1;
                    continue;
                }

                let left_name = left_field.ident.clone().unwrap().to_token_stream();

                let mut inner_names = TokenStream::new();

                if is_optional(&left_field.ty) {
                    inner_names.extend(quote::quote! { #left_name: Some(#left_name) });
                } else {
                    inner_names.extend(left_name.clone());
                }

                inner_names.extend(quote::quote! {,});

                generate(left, right, &left_name, &inner_names);

                if is_optional(&left_field.ty) {
                    left += 1;
                } else {
                    break;
                }
            }
        }
    };

    output
}

#[proc_macro_derive(AstNode, attributes(skip_item, skip_all))]
pub fn derive_ast_node(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let ident = input.ident;

    let ignore = has_attr(&input.attrs, &["skip_all"])
        .map(|attr| attr_args(attr))
        .unwrap_or((HashSet::new(), HashSet::new()));

    let params = input.generics.params;
    let where_clause = input.generics.where_clause;

    let struct_args = params.iter().map(|param| match param {
        GenericParam::Type(tp) => tp.ident.to_token_stream(),
        GenericParam::Const(tp) => tp.ident.to_token_stream(),
        GenericParam::Lifetime(tp) => tp.lifetime.to_token_stream(),
    });

    let mut num_childs = TokenStream::new();

    match &input.data {
        Data::Struct(node) => {
            num_childs.extend(derive_ast_node_helper(&ident, &node.fields, None, &ignore));
        }
        Data::Enum(node) => {
            for variant in &node.variants {
                let name = &variant.ident;
                num_childs.extend(derive_ast_node_helper(
                    &ident,
                    &variant.fields,
                    Some(name),
                    &ignore,
                ));
            }
        }
        _ => panic!("Unexpected type"),
    };

    let tokens = quote::quote! {
        impl <#params> AstNode for #ident<#(#struct_args),*> #where_clause {
            fn get_range(&self) -> Range {
                match self {
                    #num_childs
                }
            }
        }
    };

    tokens.into()
}
