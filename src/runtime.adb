with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with AST;

package body Runtime is

   function Eval(E : AST.Expr) return String is
   begin
      case E.Kind is
         when Number =>
            return To_String(E.Value);

         when String_Lit =>
            return To_String(E.Value);

         when Call_Expr =>
            if To_String(E.Name) = "ADD" then
               return Integer'Image(
                  Integer'Value(Eval(E.Args(0).all)) +
                  Integer'Value(Eval(E.Args(1).all))
               );
            else
               return "[UNKNOWN CALL]";
            end if;

         when others =>
            return "";
      end case;
   end;

   procedure Exec(P : AST.Program) is
   begin
      for I in 0 .. P.Count - 1 loop
         declare
            S : AST.Stmt := P.Stmts(I).all;
         begin
            case S.Kind is
               when Display_Stmt =>
                  Put_Line(Eval(S.Expr_Node));

               when Call_Stmt =>
                  Put_Line(Eval(S.Expr_Node));
            end case;
         end;
      end loop;
   end;

end Runtime;
