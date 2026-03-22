module
public import LeanBook.handler

public def generateGlobalCss (theme : Theme) : Array CssRule :=
  #[
    { selector := ":root", properties := [
        ("--bg-color", theme.bgColor),
        ("--text-color", theme.textColor),
        ("--font-serif", theme.fontSerif),
        ("--font-mono", theme.fontMono),
        ("--font-sans", theme.fontSans)
      ]
    },
    { selector := "body", properties := [
        ("margin", "0"),
        ("padding", "0"),
        ("background-color", "var(--bg-color)"),
        ("color", "var(--text-color)"),
        ("font-family", "var(--font-serif)"), 
        ("font-size", "18px"),
        ("line-height", "1.7"),
        ("overflow-x", "hidden")
      ]
    },
    { selector := "h1, h2, h3", properties := [
        ("font-family", "var(--font-sans)"),
        ("font-weight", "600"),
        ("margin-bottom", "1rem")
      ]
    },
    { selector := ".chapter", properties := [
        ("display", "grid"),
        ("grid-template-columns", s!"1fr minmax(auto, {theme.contentWidth}) 1fr"),
        ("grid-column-gap", "2rem"),
        ("min-height", "100vh"),
        ("padding", "4rem 0"),
        ("align-content", "start")
      ]
    },
    { selector := ".chapter > *", properties := [
        ("grid-column", "2"),
        ("margin-top", "0"),
        ("margin-bottom", "1.5rem")
      ]
    },
    { selector := "pre, code", properties := [
        ("font-family", "var(--font-mono)"),
        ("font-size", "0.9em")
      ]
    },
    { selector := ".sidenote", properties := [
        ("grid-column", "3"),
        ("font-size", "0.85em"),
        ("color", "#666666")
      ]
    }
  ]

public def myBook : Document := {
  title := "测试",
  chapters := [
    { title := "中文",
      blocks := [
        Block.para [Inline.text "这是一个生成示例，显然，这个项目完全没准备好。"],
        Block.para [
          Inline.text "行内公式：",
          Inline.math "E = mc^2",
          Inline.text "行内公式，结束。但是我需要一段足够长的中文，来查看行距，所以我就随便打了一些字。你好，世界！"
        ],
        Block.para [
          Inline.text "再来一段，看看不同段之间的段间距是否和理，",
          Inline.text "行内公式：",
          Inline.math "E = mc^2",
          Inline.text "行内公式，结束。但是我需要一段足够长的中文，来查看行距，所以我就随便打了一些字。你好，世界！"
        ],
        Block.para [Inline.text "行间公式，开始："],
        Block.math "\\int_0^\\infty e^{-x^2} dx = \\frac{\\sqrt{\\pi}}{2}",
        Block.para [Inline.text "接下来是一段 Lean 代码："],
        Block.code { code := "def hello := \"world\"", box := { wide := 65.0 } },
        Block.code { code := "#eval 1 + 1", box := { wide := 50.0 } }
      ]
    },
    { title := "English",
      blocks := [
        Block.para [Inline.text "Obversily, this project is not ready for used."],
        Block.para [
          Inline.text "A Functor ",
          Inline.math "F : \\mathcal{C} \\to \\mathcal{Set}",
          Inline.text " is representable iff there is an object ",
          Inline.math "X",
          Inline.text " such that ",
          Inline.math "C(X, -)",
          Inline.text " is isomorphic to ",
          Inline.math "F",
          Inline.text "."
        ],
        Block.para [Inline.text "hello there"],
        Block.para [
          Inline.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
        ],
        Block.para [
          Inline.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
        ]
      ]
    }
  ]
}
