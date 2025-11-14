with Ada.Text_IO; use Ada.Text_IO;
with Lexer;
with Parser;
with Runtime;

procedure Main is
   Source : String := "";
   File   : File_Type;
begin
   -- 읽기
   if Is_Open(File) then
      Close(File);
   end if;

   Open(File, In_File, "example.coba");

   while not End_Of_File(File) loop
      declare
         Line : constant String := Get_Line(File);
      begin
         -- 안전한 문자열 누적 방식
         Source := Source & ASCII.LF & Line;
      end;
   end loop;

   Close(File);

   -- 파싱
   declare
      Tokens : constant Lexer.Token_List := Lexer.Tokenize(Source);
      P      : constant Parser.Program_Type := Parser.Parse_Program(Tokens);
   begin
      Runtime.Exec(P);
   end;

end Main;
