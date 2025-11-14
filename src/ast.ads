with Ada.Strings.Unbounded;

package AST is

   type Expr_Kind is (Number, String_Lit, Ident, Call_Expr);

   type Expr;
   type Expr_Access is access Expr;

   type Arg_List is array (0 .. 9) of Expr_Access;

   type Expr is record
      Kind      : Expr_Kind;
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Value     : Ada.Strings.Unbounded.Unbounded_String;
      Args      : Arg_List;
      Arg_Count : Natural := 0;
   end record;

   type Stmt_Kind is (Display_Stmt, Call_Stmt);

   type Stmt;
   type Stmt_Access is access Stmt;

   type Stmt is record
      Kind       : Stmt_Kind;
      Expr_Node  : Expr;
   end record;

   type Stmt_List is array (0 .. 99) of Stmt_Access;

   type Program is record
      Stmts : Stmt_List;
      Count : Natural := 0;
   end record;

end AST;
