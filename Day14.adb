with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Advent_IO; use Advent_IO;

package body Day14 is

function Part1(Input: Puzzle_Input) return String is
    I, Total: Natural := 0;
    Max_Load: Natural := Natural(Input.Length);
    type Col_Idx is new Natural range 1..Length(Input.First_Element);
    Offset: array(Col_Idx) of Natural := (others => 0);
begin
    for Line of Input loop
        for J in Col_Idx loop
            case Element(Line, Natural(J)) is
                when 'O' =>
                    -- add current load to total
                    Total := Total + (Max_Load - Offset(J));
                    Offset(J) := Offset(J) + 1;
                when '#' =>
                    -- set current offset
                    Offset(J) := I+1;
                when others => null;
            end case;
        end loop;
        I := I + 1;
    end loop;
    return Total'Image;
end Part1;

function Part2(Input: Puzzle_Input) return String is
    
begin
    return "0";
end Part2;

end Day14;
