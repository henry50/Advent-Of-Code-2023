pragma Ada_2022;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Big_Numbers.Big_Reals; use Ada.Numerics.Big_Numbers.Big_Reals;
with Ada.Numerics.Big_Numbers.Big_Integers; use Ada.Numerics.Big_Numbers.Big_Integers;
with Advent_IO; use Advent_IO;

package body Day06 is

package Natural_Vec is new Ada.Containers.Vectors(
    Index_Type => Natural, Element_Type => Natural
);

function Is_Digit(C: Character) return Boolean is
begin 
    return Character'Pos(C) - Character'Pos('0') in 0..9;
end Is_Digit;

function Read_Num(S: Unbounded_String; I: in out Natural) return Natural is
    Result: Unbounded_String;
begin
    loop
        Append(Result, Element(S, I));
        I := I + 1;
        exit when I > Length(S);
        exit when not Is_Digit(Element(S, I));
    end loop;
    return Natural'Value(To_String(Result));
end Read_Num;

-- solve th-h^2-r > 0 where t = duration, h = button hold time and r = record time
function Solve_Race_Small(Duration: Natural; Best: Natural) return Natural is
    package Math renames Ada.Numerics.Elementary_Functions;
    D: Float := Float(Duration);
    B: Float := Float(Best);
    F_Soln_1: Float := (-D + Math.Sqrt(D*D - 4.0*B)) / (-2.0);
    F_Soln_2: Float := (-D - Math.Sqrt(D*D - 4.0*B)) / (-2.0);
    Soln_1: Natural := Natural(Float'Floor(Float'Min(F_Soln_1, F_Soln_2)));
    Soln_2: Natural := Natural(Float'Ceiling(Float'Max(F_Soln_1, F_Soln_2)));
begin
    return Soln_2 - Soln_1 - 1;
end Solve_Race_Small;

function Big_Sqrt(X: Big_Real) return Big_Real is
    package Converter is new Float_Conversions(Float);
    use Converter;
    Z: Big_Real := X;
    Make_4dp: Big_Real := To_Big_Real(10000);
    Big_Half: Big_Real := To_Big_Real(0.5);
begin
    for I in 1..32 loop
        Z := Big_Half * (Z+X/Z) * Make_4dp;
        -- Dodgy precision truncation to prevent storage error
        -- Truncates result to 4 decimal places
        Z := To_Big_Real(Numerator(Z)/Denominator(Z))/Make_4dp;
    end loop;
    return Z;
end Big_Sqrt;

function Solve_Race_Big(Duration: Big_Integer; Best: Big_Integer) return Natural is
    package Real_To_Float is new Float_Conversions(Float);
    use Real_To_Float;
    D: Big_Real := To_Big_Real(Duration);
    B: Big_Real := To_Big_Real(Best);
    Big_Four: Big_Real := To_Big_Real(4.0);
    Big_N_Two: Big_Real := To_Big_Real(-2.0);
    R_Soln_1: Big_Real := (-D + Big_Sqrt(D*D - Big_Four*B)) / Big_N_Two;
    R_Soln_2: Big_Real := (-D - Big_Sqrt(D*D - Big_Four*B)) / Big_N_Two;
    Max_Soln: Big_Real := Max(R_Soln_1, R_Soln_2);
    Min_Soln: Big_Real := Min(R_Soln_1, R_Soln_2);
    -- convert big real to truncated big integer
    Max_Int: Big_Integer := Numerator(Max_Soln)/Denominator(Max_Soln);
    Min_Int: Big_Integer := Numerator(Min_Soln)/Denominator(Min_Soln);
    Result: Natural := Natural(To_Integer(Max_Int - Min_Int));
begin
    return Result;
end Solve_Race_Big;

function Part1(Input: Puzzle_Input) return String is
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
        Times.Append(Read_Num(Time_Str, I));
    end loop;
    I := 1;
    while I < Length(Distance_Str) loop
        while not Is_Digit(Element(Distance_Str, I)) loop
            I := I + 1;
        end loop;
        Distances.Append(Read_Num(Distance_Str, I));
    end loop;
    -- Solve equations
    for I in Times.First_Index..Times.Last_Index loop
        Result := Result * Solve_Race_Small(Times.Element(I), Distances.Element(I));
    end loop;
    return Result'Image;
end Part1;

function Part2(Input: Puzzle_Input) return String is
    Time_Str: Unbounded_String := Input.Element(0);
    Distance_Str: Unbounded_String := Input.Element(1);
    Parsed_Time: Unbounded_String;
    Parsed_Distance: Unbounded_String;
    Time: Big_Integer;
    Distance: Big_Integer;
    I: Natural := 1;
begin
    while I <= Length(Time_Str) loop
        if Is_Digit(Element(Time_Str, I)) then
            Append(Parsed_Time, Element(Time_Str, I));
        end if;
        I := I + 1;
    end loop;
    I := 1;
    while I <= Length(Distance_Str) loop
        if Is_Digit(Element(Distance_Str, I)) then
            Append(Parsed_Distance, Element(Distance_Str, I));
        end if;
        I := I + 1;
    end loop;
    Time := From_String(To_String(Parsed_Time));
    Distance := From_String(To_String(Parsed_Distance));
    return Solve_Race_Big(Time, Distance)'Image;
end Part2;

end Day06;
