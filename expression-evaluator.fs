/// Ana Taylor

/// Since this is a course that requires a significant amount of
/// programming and it is easy to find code for many programming 
/// problems on the Internet, I want to be explicit about my expectations
/// regarding the code you submit for assignments.  I believe that you can
/// learn a lot from looking at code written by other people but that you learn
/// very little by simply copying code.  The learning objectives of this course 
/// include you learning to write and debug programs in Python and F#.  All of 
/// the code you turn in must have been written by you without immediate
/// reference to another solution to the problem you are solving. 
/// That means that you can look at other programs to see how someone solved 
/// a similar problem, but you shouldn't have any code written by someone else 
/// visible when you write yours (and you shouldn't have looked at a solution 
/// just a few seconds before you type!).  You should compose the code you write
/// based on your understanding of how the features of the language you are using
/// can be used to implement the algorithm you have chosen to solve the problem
/// you are addressing.  Doing it this way is "real programming" - in contrast 
/// to just trying to get something to work by cutting and pasting stuff you 
/// don't actually understand.  It is the only way to achieve the learning 
/// objectives of the course.


open System

/// eval function takes in (string * int) list of tuples -> string -> returns an int
let eval vars (expr : string) : int =

    /// helper function - recursive function returns the string * int tuple 
    /// finds the correct value in vars for a given target key 
    let rec find target varList : (string * int) = 
        match varList with
        |[] -> ("",0)
        |(head : (string * int)) :: tail -> let key,value = head in 
                                            if key = target then head
                                            else find target tail
    /// helper function
    /// s -> stack
    /// operations on stack with appropriate symbol, returns updated stack 
    let op symbol (s : int list) = 
        match s with
        |[] -> s
        |head :: tail-> let tailT = tail.Tail in 
                            // do the correct operations on stack depending on the operator symbol
                            if symbol = '+' then (head + tail.Head)   :: tailT 
                            elif symbol = '-' then (tail.Head - head) :: tailT 
                            elif symbol = '*' then (head * tail.Head) :: tailT 
                            else 
                                if head = 0 then s 
                                else (tail.Head / head) :: tailT 
        

    /// Stack is a list
    /// Stack contains numbers only
    let exprList = expr.ToCharArray() |> List.ofArray

    let rec innerEval (vars : (string * int) list) (stack : int list) (expre : char list) = 
        match expre with
        |[] -> stack.[0] // done, return the first element of the stack
        |head :: tail -> if head = ' ' then innerEval vars stack tail // skip spaces
                        // if it contains any of the arithmetic operators, call to op helper function to do operation on stack
                        // then pass the returned stack to innerVal with the vars and tail
                         elif "*+-/".Contains(head.ToString()) then innerEval vars (op head stack) tail
                        // if its @, create newVars by popping the top value from the stack and setting that value as the new binding for the letter
                         elif head = '@' then let newVars = (tail.[0].ToString(),stack.[0]) :: vars
                                              innerEval newVars stack tail
                        // if its #, look up the value of the letter and print out both the letter and the associated value
                         elif head = '#' then printfn "%A" (find (tail.[0].ToString()) vars)
                                              innerEval vars stack tail 
                        // else its a letter
                         else
                            let _,value = find (head.ToString()) vars // find corresponding value for a given key (head)
                            let newStack = value :: stack // append the value to stack
                            innerEval vars newStack tail  // call with new stack and tail

    // at the beginning the stack is an empty list                        
    innerEval vars [] exprList

/// MAIN with tests
[<EntryPoint>]

let main args = 

    let vars = [("a",5);("b",2);("c",9);("d",0)]

    for v in vars do
        let key, value = v
        printf "Key %A" key
        printfn " has value %A" value


    // operations
    let t1 = "ab+cb-*"
    let t2 = "cab+-"
    let t3 = "ab+"
    let t4 = "ab+cb-* @d bd+"
    let t5 = "a b     +d      / b *a+"

    let t6 = "ab-ab*ab+--"
    let t7 = "bbb @q bqbq*****" 
    let t8 = "pa pc ca- @b bc-"
    let test = [t1;t2;t3;t4;t5;t6;t7;t8]
    let result = eval vars

    for t in test do
        printf "%A evaluation results in = " t
        printfn "%A" (result t)
    0
