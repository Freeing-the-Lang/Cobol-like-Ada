with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body AST is

   type Expr_Kind is (Number, String_Lit, Ident, Call_Expr);

   type Expr is record
      Kind : Expr_Kind;
      Name : Unbounded_String := To_Unbounded_String("");
      Value : Unbounded_String := To_Unbounded_String("");
      Args : Arg_List := (others => null);
      Arg_Count : Natural := 0;
   end record;

   type Stmt_Kind is (Display_Stmt, Call_Stmt);

   type Stmt is record
      Kind : Stmt_Kind;
      Expr_Node : Expr;
   end record;

   type Program is record
      Stmts : Stmt_List := (others => null);
      Count : Natural := 0;
   end record;

end AST;
