module
public import LeanBook.handler

/-- Generate the global CSS rules for the 3-column book layout. -/
public def generateGlobalCss (theme : Theme) : Array CssRule :=
  #[
    -- CSS custom properties (design tokens)
    { selector := ":root", properties := [
        ("--bg-color",       theme.bgColor),
        ("--text-color",     theme.textColor),
        ("--font-serif",     theme.fontSerif),
        ("--font-mono",      theme.fontMono),
        ("--font-sans",      theme.fontSans),
        ("--content-width",  theme.contentWidth),
        ("--margin-width",   "22ch"),
        ("--col-gap",        "2.5rem")
      ]
    },
    -- Reset
    { selector := "*, *::before, *::after", properties := [
        ("box-sizing", "border-box"),
        ("margin",     "0"),
        ("padding",    "0")
      ]
    },
    -- Base body
    { selector := "body", properties := [
        ("background-color", "var(--bg-color)"),
        ("color",            "var(--text-color)"),
        ("font-family",      "var(--font-serif)"),
        ("font-size",        "18px"),
        ("line-height",      "1.75"),
        ("overflow-x",       "hidden")
      ]
    },
    -- 3-column CSS Grid container
    -- Columns: left-gutter | main-content | margin-notes | right-gutter
    { selector := ".chapter-grid", properties := [
        ("display",               "grid"),
        ("grid-template-columns", "1fr minmax(0, var(--content-width)) var(--margin-width) 1fr"),
        ("column-gap",            "var(--col-gap)"),
        ("padding",               "5rem 2rem 4rem"),
        ("min-height",            "100vh"),
        ("align-content",         "start")
      ]
    },
    -- Main content column
    { selector := ".col-main", properties := [
        ("grid-column",   "2"),
        ("margin-bottom", "1.5rem")
      ]
    },
    -- Margin-note column
    { selector := ".col-margin", properties := [
        ("grid-column", "3"),
        ("font-size",   "0.8em"),
        ("line-height", "1.5"),
        ("color",       "#777"),
        ("align-self",  "start"),
        ("padding-top", "0.15em")
      ]
    },
    -- Headings
    { selector := "h1, h2, h3, h4, h5, h6", properties := [
        ("font-family",   "var(--font-sans)"),
        ("font-weight",   "600"),
        ("line-height",   "1.25"),
        ("margin-bottom", "1rem")
      ]
    },
    { selector := "h1.chapter-title", properties := [
        ("font-size",      "2rem"),
        ("margin-top",     "0"),
        ("padding-bottom", "0.75rem"),
        ("border-bottom",  "1px solid #ddd")
      ]
    },
    { selector := ".chapter-number", properties := [
        ("color",     "#999"),
        ("font-size", "0.8em")
      ]
    },
    { selector := "h2.heading-lv1", properties := [
        ("font-size",  "1.5rem"),
        ("margin-top", "2.5rem")
      ]
    },
    { selector := "h3.heading-lv2", properties := [
        ("font-size",  "1.2rem"),
        ("margin-top", "2rem")
      ]
    },
    -- Paragraphs
    { selector := "p", properties := [
        ("margin-top",    "0"),
        ("text-align",    "justify"),
        ("hyphens",       "auto")
      ]
    },
    -- Display math
    { selector := ".math-display", properties := [
        ("display",         "flex"),
        ("align-items",     "center"),
        ("justify-content", "center"),
        ("position",        "relative"),
        ("padding",         "1rem 3rem"),
        ("overflow-x",      "auto")
      ]
    },
    { selector := ".math-content", properties := [
        ("flex", "1"),
        ("text-align", "center")
      ]
    },
    { selector := ".eq-number", properties := [
        ("color",         "#999"),
        ("font-size",     "0.9em"),
        ("white-space",   "nowrap"),
        ("padding-left",  "1rem")
      ]
    },
    -- Code
    { selector := "pre.code-block", properties := [
        ("background-color", "#282c34"),
        ("color",            "#abb2bf"),
        ("padding",          "1rem 1.25rem"),
        ("border-radius",    "6px"),
        ("overflow-x",       "auto"),
        ("font-family",      "var(--font-mono)"),
        ("font-size",        "0.875em"),
        ("line-height",      "1.6")
      ]
    },
    { selector := "code", properties := [
        ("font-family", "var(--font-mono)"),
        ("font-size",   "0.875em")
      ]
    },
    { selector := "p > code, li > code", properties := [
        ("background-color", "#f0f0f0"),
        ("padding",          "0.1em 0.3em"),
        ("border-radius",    "3px")
      ]
    },
    -- Theorem-like environments (base)
    { selector := ".env", properties := [
        ("padding",       "0.75rem 1.25rem"),
        ("margin-top",    "0"),
        ("margin-bottom", "1.5rem"),
        ("border-radius", "3px")
      ]
    },
    { selector := ".env-header", properties := [
        ("font-weight",   "bold"),
        ("margin-bottom", "0.5rem")
      ]
    },
    { selector := ".env-type-name", properties := [
        ("font-style", "italic")
      ]
    },
    { selector := ".env-title", properties := [
        ("font-weight", "normal"),
        ("font-style",  "normal")
      ]
    },
    -- Theorem / Lemma / Proposition / Corollary
    { selector := ".env-thm, .env-lem, .env-prop, .env-cor", properties := [
        ("background-color", "#eef3ff"),
        ("border-left",      "4px solid #3a6bc4")
      ]
    },
    -- Definition
    { selector := ".env-defn", properties := [
        ("background-color", "#fff4ee"),
        ("border-left",      "4px solid #c46a3a")
      ]
    },
    -- Example
    { selector := ".env-example", properties := [
        ("background-color", "#eeffee"),
        ("border-left",      "4px solid #3ac453")
      ]
    },
    -- Remark
    { selector := ".env-remark", properties := [
        ("background-color", "#fffdf0"),
        ("border-left",      "4px solid #c4b83a")
      ]
    },
    -- Proof (ends with tombstone ∎ via CSS)
    { selector := ".env-proof", properties := [
        ("border-left", "2px solid #bbb"),
        ("color",       "#333")
      ]
    },
    { selector := ".env-proof > .env-header", properties := [
        ("font-weight", "normal"),
        ("font-style",  "italic")
      ]
    },
    { selector := ".env-proof::after", properties := [
        ("content",    "\"∎\""),
        ("display",    "block"),
        ("text-align", "right"),
        ("color",      "#555")
      ]
    },
    -- Figures
    { selector := "figure", properties := [
        ("text-align",    "center"),
        ("margin-top",    "0"),
        ("margin-bottom", "1.5rem")
      ]
    },
    { selector := "figure img", properties := [
        ("max-width", "100%"),
        ("height",    "auto")
      ]
    },
    { selector := "figcaption", properties := [
        ("font-size",   "0.9em"),
        ("color",       "#555"),
        ("margin-top",  "0.5rem")
      ]
    },
    -- Lists
    { selector := "ul.col-main, ol.col-main", properties := [
        ("padding-left", "1.75rem")
      ]
    },
    { selector := "li", properties := [
        ("margin-bottom", "0.5rem")
      ]
    },
    -- Blockquote
    { selector := "blockquote.col-main", properties := [
        ("border-left",  "3px solid #ddd"),
        ("padding-left", "1rem"),
        ("color",        "#555")
      ]
    },
    -- Cross-references
    { selector := "a.xref", properties := [
        ("color",           "#3a6bc4"),
        ("text-decoration", "none")
      ]
    },
    { selector := "a.xref:hover", properties := [
        ("text-decoration", "underline")
      ]
    },
    -- Footnote markers
    { selector := ".footnote", properties := [
        ("font-size",      "0.75em"),
        ("vertical-align", "super"),
        ("color",          "#3a6bc4")
      ]
    },
    -- Margin notes
    { selector := ".margin-note", properties := [
        ("border-left",  "2px solid #e0e0e0"),
        ("padding-left", "0.75rem"),
        ("color",        "#666")
      ]
    },
    -- Chapter navigation bar
    { selector := ".chapter-nav", properties := [
        ("display",         "flex"),
        ("justify-content", "space-between"),
        ("align-items",     "center"),
        ("padding",         "1.5rem 2rem"),
        ("border-top",      "1px solid #eee"),
        ("max-width",       "calc(var(--content-width) + var(--margin-width) + var(--col-gap) + 4rem)"),
        ("margin",          "0 auto")
      ]
    },
    { selector := ".nav-link", properties := [
        ("color",           "#3a6bc4"),
        ("text-decoration", "none"),
        ("font-size",       "0.95em")
      ]
    },
    { selector := ".nav-link:hover", properties := [
        ("text-decoration", "underline")
      ]
    },
    -- Help button (fixed corner)
    { selector := ".help-btn", properties := [
        ("position",         "fixed"),
        ("bottom",           "1.5rem"),
        ("right",            "1.5rem"),
        ("width",            "2.25rem"),
        ("height",           "2.25rem"),
        ("border-radius",    "50%"),
        ("background-color", "#f0f0f0"),
        ("border",           "1px solid #ccc"),
        ("cursor",           "pointer"),
        ("font-size",        "1rem"),
        ("color",            "#777"),
        ("opacity",          "0.55"),
        ("transition",       "opacity 0.2s"),
        ("display",          "flex"),
        ("align-items",      "center"),
        ("justify-content",  "center")
      ]
    },
    { selector := ".help-btn:hover", properties := [
        ("opacity", "1")
      ]
    },
    -- Inline math
    { selector := ".math-inline", properties := [
        ("display",        "inline"),
        ("white-space",    "nowrap")
      ]
    }
  ]

/-- Example book demonstrating the new AST. -/
public def myBook : Document := {
  title  := [Inline.text "LeanBook Demo"],
  author := "Anonymous",
  chapters := [
    { title  := [Inline.text "Introduction"],
      label  := some "ch-intro",
      blocks := [
        Block.para [
          Inline.text "Welcome to ",
          Inline.bold [Inline.text "LeanBook"],
          Inline.text ", a static-site generator for mathematical and technical books."
        ],
        Block.para [
          Inline.text "Inline math: ",
          Inline.math "E = mc^2",
          Inline.text ".  Display math follows:"
        ],
        Block.displayMath "\\int_0^\\infty e^{-x^2}\\,dx = \\frac{\\sqrt{\\pi}}{2}"
          (some "eq-gaussian"),
        Block.marginNote [
          Block.para [Inline.text "This is a margin note aligned with the display math above."]
        ],
        Block.env .thm
          (some [Inline.text "Yoneda Lemma"])
          (some "thm-yoneda")
          [Block.para [
            Inline.text "Let ",
            Inline.math "F : \\mathcal{C} \\to \\mathbf{Set}",
            Inline.text " be a functor.  Then ",
            Inline.math "F \\cong \\mathcal{C}(X, -)",
            Inline.text " for some object ",
            Inline.math "X \\in \\mathcal{C}",
            Inline.text "."
          ]],
        Block.env .proof none none [
          Block.para [
            Inline.text "Apply the Yoneda embedding and check naturality."
          ]
        ],
        Block.heading 1 [Inline.text "Code Example"] (some "sec-code"),
        Block.para [Inline.text "Here is a Lean 4 snippet:"],
        Block.codeBlock "lean"
          "def hello : String := \"Hello, world!\"\n#eval hello",
        Block.para [
          Inline.text "For more details see ",
          Inline.ref "ch-math",
          Inline.text "."
        ]
      ]
    },
    { title  := [Inline.text "Mathematical Foundations"],
      label  := some "ch-math",
      blocks := [
        Block.para [
          Inline.text "This chapter reviews the mathematical background."
        ],
        Block.env .defn
          (some [Inline.text "Category"])
          (some "defn-category")
          [Block.para [
            Inline.text "A ",
            Inline.emph [Inline.text "category"],
            Inline.text " consists of objects, morphisms, composition, and identity satisfying the associativity and identity laws."
          ]],
        Block.heading 1 [Inline.text "Limits and Colimits"] none,
        Block.para [
          Inline.text "A ",
          Inline.emph [Inline.text "limit"],
          Inline.text " of a diagram ",
          Inline.math "D : \\mathcal{J} \\to \\mathcal{C}",
          Inline.text " is a universal cone over ",
          Inline.math "D",
          Inline.text "."
        ],
        Block.marginNote [
          Block.para [Inline.text "Limits generalise products, pullbacks, and equalisers."]
        ],
        Block.displayMath
          "\\lim_{\\leftarrow} D \\cong \\int_{j \\in \\mathcal{J}} D(j)"
          none,
        Block.env .remark none none [
          Block.para [Inline.text "Colimits are the dual notion, obtained by reversing all arrows."]
        ],
        Block.list
          [ [Block.para [Inline.text "Products are limits over the discrete diagram."]],
            [Block.para [Inline.text "Equalisers are limits over the parallel-arrows diagram."]],
            [Block.para [Inline.text "Pullbacks are limits over the cospan diagram."]] ]
          false
      ]
    }
  ]
}
