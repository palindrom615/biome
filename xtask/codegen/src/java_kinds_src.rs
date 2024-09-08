use crate::kind_src::KindsSrc;

pub const JAVA_KINDS_SRC: KindsSrc = KindsSrc {
    punct: &[
        (":", "COLON"),
        (",", "COMMA"),
        ("{", "L_CURLY"),
        ("}", "R_CURLY"),
        ("[", "L_BRACK"),
        ("]", "R_BRACK"),
        ("-", "DASH"),
        ("%", "PERCENT"),
        ("*", "STAR"),
        ("#", "HASH"),
        ("!", "BANG"),
        ("@", "AT"),
        ("<<", "SHL"),
        ("&", "AMP"),
        ("|", "PIPE"),
        (">", "R_ANGLE"),
        ("~", "TILDE"),
        ("`", "BACKTICK"),
        ("---", "DOC_START"),
        ("...", "DOC_END"),
    ],
    keywords: &["null"],
    literals: &["JAVA_STRING_LITERAL", "JAVA_SCALAR"],
    tokens: &["NEWLINE", "WHITESPACE", "IDENT", "COMMENT"],
    nodes: &[
        "JAVA_ROOT",
        "JAVA_PACKAGE",
        "JAVA_IMPORT_LIST",
        "JAVA_IMPORT",
        "JAVA_CLASS_LIST",
        "JAVA_CLASS",

        // Bogus nodes
        "JAVA_BOGUS",
        "JAVA_BOGUS_VALUE",
    ],
};
