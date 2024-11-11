use std::ops::ControlFlow;

use super::astnodes::ExprNode;

pub(super) type ExprFlow<'i> = ControlFlow<ExprNode<'i>, ExprNode<'i>>;

pub(super) trait Converge<T> {
    fn converge(self) -> T;
    fn boxed(self) -> impl Converge<Box<T>>;
}

impl<T> Converge<T> for ControlFlow<T, T> {
    fn converge(self) -> T {
        match self {
            ControlFlow::Break(value) => value,
            ControlFlow::Continue(value) => value,
        }
    }

    #[allow(refining_impl_trait)]
    fn boxed(self) -> ControlFlow<Box<T>, Box<T>> {
        match self {
            ControlFlow::Break(value) => ControlFlow::Break(Box::new(value)),
            ControlFlow::Continue(value) => ControlFlow::Continue(Box::new(value)),
        }
    }
}

pub(super) fn converge_tracked<T>(value: ControlFlow<T, T>, continue_flag: &mut bool) -> T {
    *continue_flag = *continue_flag && matches!(value, ControlFlow::Continue(_));
    value.converge()
}

macro_rules! converge_expr {
    ($ExprType:path => { $($cfName:ident: $cfVal:expr, )* } $(!{ $($name:ident: $val:expr, )* })?) => {{
        let mut continue_flag = true;
        let expr = {
            $ExprType {
                $($cfName: $crate::ast::controlflow::converge_tracked($cfVal, &mut continue_flag),)*
                $($($name: $val,)*)?
            }
        };

        if continue_flag {
            std::ops::ControlFlow::Continue(expr)
        } else {
            std::ops::ControlFlow::Break(expr)
        }
    }};

    ($ExprType:path => ( $($val:expr),* )) => {{
        let mut continue_flag = true;
        let expr = $ExprType(
            $(crate::ast::controlflow::converge_tracked($val, &mut continue_flag)),*
        );

        if continue_flag {
            std::ops::ControlFlow::Continue(expr)
        } else {
            std::ops::ControlFlow::Break(expr)
        }
    }};
}

pub(super) use converge_expr;
