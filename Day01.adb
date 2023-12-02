with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Advent_IO; use Advent_IO;

package body Day01 is

function Part1(Input: Puzzle_Input) return Integer is
    Total: Integer := 0;
    Elem: Character;
    Elem_Int: Integer;
    First: Integer := -1;
    Last: Integer;
    -- Output: Unbounded_String;
begin
    for Line of Input loop
        -- For each character in the string
        for I in 1 .. Length(Line) loop
            -- Get the character at that position
            Elem := Element(Line, I);
            -- Work out its Integer value
            Elem_Int := Character'Pos(Elem) - Character'Pos('0');
            -- If the Integer value is a digit either set the
            -- first or last values
            if Elem_Int in 0..9 then
                if First = -1 then
                    First := 10 * Elem_Int;
                    Last := Elem_Int;
                else
                    Last := Elem_Int;
                end if;
            end if;
        end loop;
        -- Add to total
        Total := Total + First + Last;
        First := -1;
    end loop;
    return Total;
end Part1;

function Part2(Input: Puzzle_Input) return Integer is
    -- Output: Unbounded_String;
begin
    return Integer(Input.Length);
end Part2;

end Day01;