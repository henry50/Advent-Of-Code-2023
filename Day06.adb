with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Numerics.Elementary_Functions;
with Advent_IO; use Advent_IO;

package body Day06 is

package Natural_Vec is new Ada.Containers.Vectors(
    Index_Type => Natural, Element_Type => Natural
);

function Is_Digit(C: Character) return Boolean is
begin 
    return Character'Pos(C) - Character'Pos('0') in 0..9;
end Is_Digit;

function Read_Num(S: Unbounded_String; I: in out Natural; Ignore_Space: Boolean) return Natural is
    Result: Unbounded_String;
begin
    loop
        Append(Result, Element(S, I));
        I := I + 1;
        exit when I > Length(S);
        while Ignore_Space and Element(S, I) = ' ' loop
            I := I + 1;
        end loop;
        exit when not Is_Digit(Element(S, I));
    end loop;
    return Natural'Value(To_String(Result));
end Read_Num;

-- solve th-h^2-r > 0 where t = duration, h = button hold time and r = record time
-- -x^2+tx-r > 0, a = -1, b = t, c = -r
function Solve_Race(Duration: Natural; Best: Natural) return Natural is
    package Math renames Ada.Numerics.Elementary_Functions;
    D: Float := Float(Duration);
    B: Float := Float(Best);
    F_Soln_1: Float := (-D + Math.Sqrt(D*D - 4.0*B)) / (-2.0);
    F_Soln_2: Float := (-D - Math.Sqrt(D*D - 4.0*B)) / (-2.0);
    Soln_1: Natural := Natural(Float'Floor(Float'Min(F_Soln_1, F_Soln_2)));
    Soln_2: Natural := Natural(Float'Ceiling(Float'Max(F_Soln_1, F_Soln_2)));
begin
    return Soln_2 - Soln_1 - 1;
end Solve_Race;

function Part1(Input: Puzzle_Input) return Integer is
    Time_Str: Unbounded_String := Input.Element(0);
    Distance_Str: Unbounded_String := Input.Element(1);
    Times: Natural_Vec.Vector;
    Distances: Natural_Vec.Vector;
    Result: Natural := 1;
    I: Natural := 1;
begin
    -- Parse input
    while I < Length(Time_Str) loop
        while not Is_Digit(Element(Time_Str, I)) loop
            I := I + 1;
        end loop;
        Times.Append(Read_Num(Time_Str, I, False));
    end loop;
    I := 1;
    while I < Length(Distance_Str) loop
        while not Is_Digit(Element(Distance_Str, I)) loop
            I := I + 1;
        end loop;
        Distances.Append(Read_Num(Distance_Str, I, False));
    end loop;
    -- Solve equations
    for I in Times.First_Index..Times.Last_Index loop
        Result := Result * Solve_Race(Times.Element(I), Distances.Element(I));
    end loop;
    return Result;
end Part1;

function Part2(Input: Puzzle_Input) return Integer is
    Time_Str: Unbounded_String := Input.Element(0);
    Distance_Str: Unbounded_String := Input.Element(1);
    Time: Natural;
    Distance: Natural;
    I: Natural := 1;
begin
    while I < Length(Time_Str) loop
        while not Is_Digit(Element(Time_Str, I)) loop
            I := I + 1;
        end loop;
        Time := Read_Num(Time_Str, I, True);
    end loop;
    I := 1;
    while I < Length(Distance_Str) loop
        while not Is_Digit(Element(Distance_Str, I)) loop
            I := I + 1;
        end loop;
        Distance := Read_Num(Distance_Str, I, True);
    end loop;
    return Solve_Race(Time, Distance);
end Part2;

end Day06;
