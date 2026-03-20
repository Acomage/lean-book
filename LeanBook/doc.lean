module

public structure Box where
  wide : Float

public structure CodeBlock where
  code : String
  box : Box

public inductive Inline where
  | text (s : String)
  | math (tex : String)

public inductive Block where
  | para (inlines : List Inline)
  | math (tex : String)
  | code (cb : CodeBlock)

public structure Chapter where
  title : String
  blocks : List Block

public structure Document where
  title : String
  chapters : List Chapter

