with Ada.Strings.Unbounded;

package Lexer is

   type Token_Kind is (
      Number_Lit,
      String_Lit,
      Ident,
      Dot,
      LParen,
      RParen,
      Comma,
      EOF_Token
   );

   type Token is record
      Kind : Token_Kind;
      Text : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Token_List is array (0 .. 999) of Token;

   -- main entry
   function Tokenize(Source : String) return Token_List;

end Lexer;
