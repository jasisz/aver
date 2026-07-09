fn expr_fragment_ty_from_wasm_param(ty: &TyKind, carrier: u32) -> Option<FragTy> {
    match ty {
        TyKind::F64 => Some(FragTy::F64),
        TyKind::I32 => Some(FragTy::BoolI32),
        TyKind::Ref(idx) if *idx == carrier => Some(FragTy::IntCarrier),
        _ => None,
    }
}

fn expr_fragment_ty_from_wasm_result(ty: TyKind) -> Option<FragTy> {
    match ty {
        TyKind::F64 => Some(FragTy::F64),
        TyKind::I32 => Some(FragTy::BoolI32),
        _ => None,
    }
}
