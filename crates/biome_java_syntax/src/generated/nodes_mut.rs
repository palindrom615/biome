//! Generated file, do not edit by hand, see `xtask/codegen`

use crate::{generated::nodes::*, JavaSyntaxToken as SyntaxToken};
use biome_rowan::AstNode;
use std::iter::once;
impl JavaRoot {
    pub fn with_value_token(self, element: SyntaxToken) -> Self {
        Self::unwrap_cast(
            self.syntax
                .splice_slots(0usize..=0usize, once(Some(element.into()))),
        )
    }
}
