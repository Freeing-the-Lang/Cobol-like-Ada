with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Lexer is

   function Tokenize(Source : String) return Token_List is
      Tokens : Token_List := (others => (Kind => EOF_Token, Text => To_Unbounded_String("")));
      Index  : Natural := 1;

      Pos : Positive := 1;
      Len : Natural := Source'Length;

      procedure Add_Token(K : Token_Kind; T : String) is
      begin
         Tokens(Index).Kind := K;
         Tokens(Index).Text := To_Unbounded_String(T);
         Index := Index + 1;
      end Add_Token;

   begin
      while Pos <= Len loop
         declare
            C : Character := Source(Pos);
         begin
            if C = ' ' or C = ASCII.LF then
               Pos := Pos + 1;
            elsif C = '"' then
               declare
                  Str : String := "";
                  P   : Positive := Pos + 1;
               begin
                  while P <= Len and Source(P) /= '"' loop
                     Str := Str & Source(P);
                     P := P + 1;
                  end loop;

                  Add_Token(String_Lit, Str);
                  Pos := P + 1;
               end;
            elsif C in '0' .. '9' then
               declare
                  Num : String := "";
                  P   : Positive := Pos;
               begin
                  while P <= Len and then Source(P) in '0'..'9' loop
                     Num := Num & Source(P);
                     P := P + 1;
                  end loop;

                  Add_Token(Number_Lit, Num);
                  Pos := P;
               end;
            elsif C = '.' then
               Add_Token(Dot, ".");
               Pos := Pos + 1;
            elsif C = '(' then
               Add_Token(LParen, "(");
               Pos := Pos + 1;
            elsif C = ')' then
               Add_Token(RParen, ")");
               Pos := Pos + 1;
            elsif C = ',' then
               Add_Token(Comma, ",");
               Pos := Pos + 1;
            else
               declare
                  Id : String := "";
                  P  : Positive := Pos;
               begin
                  while P <= Len and then Source(P) not in " .()," loop
                     Id := Id & Source(P);
                     P := P + 1;
                  end loop;

                  Add_Token(Ident, Id);
                  Pos := P;
               end;
            end if;
         end;
      end loop;

      Add_Token(EOF_Token, "");
      return Tokens;
   end Tokenize;

end Lexer;
