module
public import LeanBook.html
public import LeanBook.doc

public class Handle (α : Type) (β : outParam Type) where
  handle : α → RenderM β

namespace CodeBlock
-- 局部重写 Box：改为黑底白字，生成带有特定样式的 class
public scoped instance : Handle Box (Html → Html) where
  handle b := do
    let st ← get
    let id := st.nextId
    let cName := s!"code-box-{id}"
    
    let rule : CssRule := {
      className := cName,
      properties := [("width", s!"{b.wide}ch"), ("background-color", "#000000"), ("color", "#FFFFFF")]
    }
    set { st with rules := st.rules.push rule, nextId := id + 1 }
    return fun childHtml => Html.element "div" [("class", cName)] [childHtml]

public instance : Handle CodeBlock Html where
  handle cb := do
    -- 在 Monad 中依次调用
    let boxWrapper ← Handle.handle (β := Html → Html) cb.box
    let codeHtml := Html.element "pre" [] [Html.element "code" [] [Html.text cb.code]]
    return boxWrapper codeHtml
end CodeBlock

-- 基础节点的实现 (使用 pure 或 mapM)
public instance : Handle Inline Html where
  handle
    | .text s => pure <| Html.element "span" [] [Html.text s]
    | .math tex => pure <| Html.element "span" [("class", "math inline"), ("data-source", s!"${tex}$")] [Html.text s!"\\({tex}\\)"]

public instance : Handle Block Html where
  handle
    | .para inlines => do
        let children ← inlines.mapM (Handle.handle (β := Html))
        return Html.element "p" [] children
    | .math tex => pure <| Html.element "div" [("class", "math display"), ("data-source", s!"$${tex}$$")] [Html.text s!"\\[{tex}\\]"]
    | .code cb => Handle.handle cb

public instance : Handle Chapter Html where
  handle ch := do
    let titleHtml := Html.element "h2" [("class", "chapter-title")] [Html.text ch.title]
    let blocksHtml ← ch.blocks.mapM (Handle.handle (β := Html))
    return Html.element "section" [("class", "chapter")] (titleHtml :: blocksHtml)

-- Document 渲染：在此处将收集到的所有 CSS 注入到 <style> 标签中！
public instance : Handle Document Html where
  handle doc := do
    -- 1. 先遍历所有章节，这会触发所有底层节点的 handle 并填充 State
    let chaptersHtml ← doc.chapters.mapM (Handle.handle (β := Html))
    
    -- 2. 获取当前收集到的所有 CSS 规则
    let st ← get
    let dynamicCss := renderCss st.rules
    
    -- 3. 将动态 CSS 注入到 <style> 标签
    let head := Html.element "head" [] [
      Html.element "meta" [("charset", "UTF-8")] [],
      Html.element "title" [] [Html.text doc.title],
      Html.element "link" [("rel", "stylesheet"), ("href", "style.css")] [], -- 这里放基础三栏布局的静态CSS
      Html.element "style" [] [Html.text dynamicCss],                        -- 这里放由代码生成的动态CSS
      Html.element "script" [("id", "MathJax-script"), ("async", "true"), ("src", "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")] []
    ]
    let body := Html.element "body" [] [
      Html.element "main" [("class", "main-content")] chaptersHtml,
      Html.element "script" [("src", "main.js")] []
    ]
    return Html.element "html" [("lang", "en")] [head, body]
