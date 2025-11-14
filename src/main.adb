with Ada.Text_IO; use Ada.Text_IO;
with Lexer;
with Parser;
with Runtime;

procedure Main is
   Source : String := "";
   File   : File_Type;
begin
   -- Read example.coba
   Open(File, In_File, "example.coba");

   while not End_Of_File(File) loop
      declare
         L : constant String := Get_Line(File);
      begin
         -- Safe string accumulation
         Source := Source & ASCII.LF & L;
      end;
   end loop;

   Close(File);

   declare
      Tokens : constant Lexer.Token_List   := Lexer.Tokenize(Source);
      Prog   : constant Parser.Program_Type := Parser.Parse_Program(Tokens);
   begin
      Runtime.Exec(Prog);
   end;

end Main;
