with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Directories; use Ada.Directories;

package body Advent_IO is



function Get_Input(Filename: String) return Puzzle_Input is
    File: File_Type;
    Line: Unbounded_String;
    Output: Puzzle_Input;
begin
    if not Exists(Filename) then
        raise File_Not_Found_Error with "Could not open input file: " & Filename & " does not exist";
    end if;
    Open(File, In_File, Filename);
    loop
        exit when End_Of_File(File);
        Get_Line(File, Line);
        Output.Append(Line);
    end loop;
    return Output;
end Get_Input;

end Advent_IO;