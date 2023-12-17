with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Advent_IO; use Advent_IO;

package body Day02 is

type Sub_Game is record
    Red: Natural := 0;
    Green: Natural := 0;
    Blue: Natural := 0;
end record;

package Game_Vec is new Ada.Containers.Vectors
    (Index_Type   => Natural,
    Element_Type => Sub_Game);

type Game is record
    Num: Natural;
    Sub_Games: Game_Vec.Vector;
end record;

function Parse_Game(Game_Str: Unbounded_String) return Game is
    Result: Game;
    I: Natural := 1;
    Current_Sub_Game: Sub_Game;
    Colour_Num: Natural;
    procedure Consume is
    begin
        I := I + 1;
    end Consume;
    procedure Consume(N: Natural) is
    begin
        I := I + N;
    end Consume;
    function Read_Num return Natural is
        Result: Unbounded_String;
    begin
        loop
            Append(Result, Element(Game_Str, I));
            Consume;
            exit when not (Character'Pos(Element(Game_Str, I)) - Character'Pos('0') in 0..9);
        end loop;
        return Natural'Value(To_String(Result));
    end Read_Num;
    ParseError: exception;
begin
    Consume(5); -- "Game "
    -- Get game number
    Result.Num := Read_Num;
    Consume(2); -- ": "
    -- for each sub-game
    loop
        Current_Sub_Game := (Red => 0, Green => 0, Blue => 0);
        -- for each colour
        loop
            Colour_Num := Read_Num;
            Consume; -- space
            case Element(Game_Str, I) is
                when 'r' =>
                    Current_Sub_Game.Red := Colour_Num;
                    Consume(3); -- red
                when 'g' =>
                    Current_Sub_Game.Green := Colour_Num;
                    Consume(5); -- green
                when 'b' =>
                    Current_Sub_Game.Blue := Colour_Num;
                    Consume(4); -- blue
                when others => raise ParseError with "Expected rgb";
            end case;
            exit when I > Length(Game_Str);
            exit when Element(Game_Str, I) = ';';
            Consume(2); -- ", "
        end loop;
        Consume(2); -- "; "
        Result.Sub_Games.Append(Current_Sub_Game);
        exit when I > Length(Game_Str);
    end loop;
    return Result;
end Parse_Game;

function Part1(Input: Puzzle_Input) return String is
    Current_Game: Game;
    Current_Game_Invalid: Boolean;
    Total: Natural := 0;
begin
    for Line of Input loop
        Current_Game := Parse_Game(Line);
        Current_Game_Invalid := False;
        for Current_Sub_Game of Current_Game.Sub_Games loop
            if Current_Sub_Game.Red   > 12
            or Current_Sub_Game.Green > 13
            or Current_Sub_Game.Blue  > 14 then
                Current_Game_Invalid := True;
                exit; -- stop considering this game
            end if;
        end loop;
        if not Current_Game_Invalid then
            Total := Total + Current_Game.Num;
        end if;
    end loop;
    return Total'Image;
end Part1;

function Part2(Input: Puzzle_Input) return String is
    Current_Game: Game;
    Max_Red: Natural;
    Max_Green: Natural;
    Max_Blue: Natural;
    Total: Natural := 0;
begin
    for Line of Input loop
        Current_Game := Parse_Game(Line);
        Max_Red := 0; Max_Green := 0; Max_Blue := 0;
        for Current_Sub_Game of Current_Game.Sub_Games loop
            if Current_Sub_Game.Red > Max_Red then
                Max_Red := Current_Sub_Game.Red;
            end if;
            if Current_Sub_Game.Green > Max_Green then
                Max_Green := Current_Sub_Game.Green;
            end if;
            if Current_Sub_Game.Blue > Max_Blue then
                Max_Blue := Current_Sub_Game.Blue;
            end if;
        end loop;
        Total := Total + (Max_Red * Max_Green * Max_Blue);
    end loop;
    return Total'Image;
end Part2;

end Day02;
