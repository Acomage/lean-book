module
public import LeanBook.handler

-- 5. 实例化一个包含两章、文本、公式和代码的文档
public def myBook : Document := {
  title := "测试",
  chapters := [
    { title := "中文",
      blocks := [
        Block.para [Inline.text "这是一个生成示例，显然，这个项目完全没准备好。"],
        Block.para [
          Inline.text "行内公式，启动！！！！！",
          Inline.math "E = mc^2",
          Inline.text "行内公式，结束。"
        ],
        Block.para [Inline.text "行间公式，启动！！！！！"],
        Block.math "\\int_0^\\infty e^{-x^2} dx = \\frac{\\sqrt{\\pi}}{2}",
        Block.para [Inline.text "接下来是一段 Lean 代码："],
        Block.code { code := "def hello := \"world\"", box := { wide := 65.0 } }
      ]
    },
    { title := "English",
      blocks := [
        Block.para [Inline.text "Obversily, this project is not ready for used."],
        Block.para [
          Inline.text "A Functor ",
          Inline.math "F : \\mathcal{C} \\to \\mathcal{D}",
          Inline.text "is representable iff ..."
        ],
        Block.para [Inline.text "hello there"]
      ]
    }
  ]
}
