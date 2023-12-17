with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Advent_IO; use Advent_IO;

package body Day01 is

function Part1(Input: Puzzle_Input) return String is
    Total: Integer := 0;
    Elem: Character;
    Elem_Int: Integer;
    First: Integer := -1;
    Last: Integer;
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
    return Total'Image;
end Part1;

function Part2(Input: Puzzle_Input) return String is
    Total: Integer := 0;
    First: Integer := -1;
    Last: Integer;
    type Direction is (Left, Right);

    function Contains(Whole: String; Part: String) return Boolean is
    begin
        -- A string cannot contain a string longer than it
        if Part'Length > Whole'Length then
            return False;
        end if;
        -- Check if every offset matches the part
        for Offset in 1..Whole'Length - Part'Length + 1 loop
            if Whole(Offset..Offset + Part'Length - 1) = Part then
                return True;
            end if;
        end loop;
        return False;
    end Contains;

    function Match(Substr: String) return Integer is
    begin
        -- Try and match this string to each number word
        if Contains(Substr, "one") then
            return 1;
        elsif Contains(Substr, "two") then
            return 2;
        elsif Contains(Substr, "three") then
            return 3;
        elsif Contains(Substr, "four") then
            return 4;
        elsif Contains(Substr, "five") then
            return 5;
        elsif Contains(Substr, "six") then
            return 6;
        elsif Contains(Substr, "seven") then
            return 7;
        elsif Contains(Substr, "eight") then
            return 8;
        elsif Contains(Substr, "nine") then
            return 9;
        end if;
        return -1;
    end Match;

    function First_Match(Line: Unbounded_String; Start_At: Direction) return Integer is
        Elem: Character;
        Elem_Int: Integer;
    begin
        -- Decide whether to start at the beginning or end
        case Start_At is
        when Left =>
            for I in 1 .. Length(Line) loop
                -- This bit is similar to part 1
                Elem := Element(Line, I);
                Elem_Int := Character'Pos(Elem) - Character'Pos('0');
                if Elem_Int in 1..9 then
                    return Elem_Int;
                end if;
                -- If the element is not a digit try and 
                -- match it to a word
                declare
                    Substr: String := Slice(Line, 1, I);
                begin
                    Elem_Int := Match(SubStr);
                    if Elem_Int /= -1 then
                        return Elem_Int;
                    end if;
                end;
            end loop;
        when Right =>
            -- Same as Left but in reverse
            for I in reverse 1 .. Length(Line) loop
                Elem := Element(Line, I);
                Elem_Int := Character'Pos(Elem) - Character'Pos('0');
                if Elem_Int in 1..9 then
                    return Elem_Int;
                end if;
                declare
                    -- It is VERY important to specify the values
                    -- used to index this String or it will break
                    -- everything in Contains (I wasted far too long on this)
                    Substr: String(1..Length(Line)-I+1) := Slice(Line, I, Length(Line));
                begin
                    Elem_Int := Match(SubStr);
                    if Elem_Int /= -1 then
                        return Elem_Int;
                    end if;
                end;
            end loop;
        end case;
        return -1;
    end First_Match;
begin
    for Line of Input loop
        -- Get the first and last values and add them to the total
        First := 10 * First_Match(Line, Left);
        Last := First_Match(Line, Right);
        Total := Total + First + Last;
    end loop;
    return Total'Image;
end Part2;

end Day01;