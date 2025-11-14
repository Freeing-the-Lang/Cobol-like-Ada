with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Lexer;
with Parser;
with Runtime;
with AST;  -- ★★ 핵심! visibility 에러 해결

procedure Main is
   Source_UB : Unbounded_String := To_Unbounded_String("");
   File      : File_Type;
begin
   Open(File, In_File, "example.coba");

   while not End_Of_File(File) loop
      declare
         L : constant String := Get_Line(File);
      begin
         -- ★ 안전한 방식 (경고 없음)
         Source_UB := Source_UB & ASCII.LF & L;
      end;
   end loop;

   Close(File);

   declare
      Source : constant String             := To_String(Source_UB);
      Tokens : constant Lexer.Token_List   := Lexer.Tokenize(Source);
      Prog   : constant AST.Program        := Parser.Parse_Program(Tokens);
   begin
      Runtime.Exec(Prog);
   end;

end Main;
