with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
package Advent_IO is
    type Day is new Natural range 1..25;
    package Unbounded_String_Vector is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Unbounded_String);
    File_Not_Found_Error: exception;
    type Puzzle_Input is new Unbounded_String_Vector.Vector with null record;
    function Get_Input(Filename: String) return Puzzle_Input;
end Advent_IO;