module MoodleQuiz

using LightXML

import Base.convert
import Base.print
import Base.show
import Base.Random: uuid1, UUID

export QuestionType, Question, Answer, MoodleText, MoodleTextFormat, Quiz, TrueFalseAnswer, exportXML, @M_str, @M_mstr
export MultipleChoice, WeightedMultipleChoice, TrueFalse, ShortAnswer, Matching, EmbeddedAnswers, Essay, Numerical, Description, CalculatedSimple, DragAndDrop, DragAndDropMatch, AllOrNothingMultipleChoice, Stack
export NumericalEmbeddedAnswer, EmbeddedAnswer, EmbeddedAnswerOption, ShortAnswerCaseInsensitive, ShortAnswerCaseSensitive, NumericalAnswer, TrueFalseEmbeddedAnswer, MultipleChoiceSelect, MultipleChoiceVertical, MultipleChoiceHorizontal, DragAndDropOption
export MatrixEmbeddedAnswer
export MoodleFile, EmbedFile
export StackInput, StackAnswerTest, PRTree, PRTNode, EmbedInput
export StackInputType, AlgebraicInput, CheckboxInput, DropdownInput, CharacterInput, MatrixInput, RadioButtonInput, TextBoxInput, UnitsInput, TrueFalseInput

"""
Moodle supports the following types of questions

 Type                        | Description
 :---------------------------| :---------------------------------------------------------------------------------------------------------
 MultipleChoice              | standard multiple choice question
 WeightedMultipleChoice      | Multiple choice with weighted grades for answers by their fractions; careful: moodle only allows certain fractions (1/n for n= 1,...,10,20 and some multiples...)
 AllOrNothingMultipleChoice  | multiple choice question, where all correct and no false answers have to be selected to recieve any points
 TrueFalse                   | simple yes / no question
 ShortAnswer                 | not yet implemented
 Matching                    | not yet implemented
 EmbeddedAnswers             | answer fields / options are embedded in question text
 Essay                       | not yet implemented
 Numerical                   | not yet implemented
 Description                 | not yet implemented
 CalculatedSimple            | not yet implemented
 DragAndDrop                 | not yet implemented
 DragAndDropMatch            | not yet implemented
 Stack                       | Uses the Maxima CAS engine to check algebraic correctness of solution
"""
@enum QuestionType MultipleChoice WeightedMultipleChoice TrueFalse ShortAnswer Matching EmbeddedAnswers Essay Numerical Description CalculatedSimple DragAndDrop DragAndDropMatch AllOrNothingMultipleChoice Stack
convert(::Type{AbstractString},x::QuestionType) = (
   Dict(
    # MultipleChoice => "multichoice",
    MultipleChoice => "oumultiresponse",
    WeightedMultipleChoice => "multichoice",
    AllOrNothingMultipleChoice => "multichoiceset",
    TrueFalse => "truefalse",
    ShortAnswer => "shortanswer",
    Matching => "matching",
    EmbeddedAnswers => "cloze",
    Essay => "essay",
    Numerical => "numerical",
    Description => "description",
    CalculatedSimple => "calculatedsimple",
    DragAndDrop => "ddimageortext",
    DragAndDropMatch => "ddmatch",
    Stack => "stack"
  )[x]
)

"""
Text in a moodle quiz can have one of the following formats

 Format                  | Descriction
 :-----------------------| :----------------------------------------------------------------------------------------------------------------------
 Html                    |
 MoodleAutoFormat        | some keywords as e.g. true / false will be automatically translated to the language of the moodle user viewing the quiz
 PlainText               |
 Markdown                |
"""
@enum MoodleTextFormat HTML MoodleAutoFormat PlainText Markdown
convert(::Type{AbstractString},format::MoodleTextFormat) = (
  get(Dict(
    HTML => "html",
    MoodleAutoFormat => "moodle_auto_format",
    PlainText => "plain_text",
    Markdown => "markdown"
    ), format, "")
)

"""
An inline answer in a moodle quiz can have one of the following types

 Type                     | Descriction
 :------------------------| :----------------------------------------------------------------------------------------------------------------------
 ShortAnswer              | standard text field, case insensitive
 ShortAnswerCaseSensitive | standard text field, case sensitive
 NumericalAnswer          | numerical value
 MultipleChoiceSelect     | select field with multiple options
 MultipleChoiceVertical   | options represented as a column of radio buttons
 MultipleChoiceHorizontal | options represented as a row of radio buttons
"""
@enum EmbeddedAnswerType ShortAnswerCaseInsensitive ShortAnswerCaseSensitive NumericalAnswer MultipleChoiceSelect MultipleChoiceVertical MultipleChoiceHorizontal
convert(::Type{AbstractString},itype::EmbeddedAnswerType) = (
  get(Dict(
    ShortAnswerCaseInsensitive => "SHORTANSWER",
    ShortAnswerCaseSensitive => "SHORTANSWER_C",
    NumericalAnswer => "NUMERICAL",
    MultipleChoiceSelect => "MULTICHOICE",
    MultipleChoiceVertical => "MULTICHOICE_V",
    MultipleChoiceHorizontal => "MULTICHOICE_H"
    ),itype,"")
)

"""
The check for correctness in Stack Questions can be any of the following
  Type    | Description
  ---------------
  AlgebraicEquivalence | Algebraic Equivalence (using simplifications)

  Others are not tested or understood!
"""
@enum StackAnswerTest AlgebraicEquivalence CasEqual CompSquare Diff EqualComAss Expanded FacForm IntEquiv LowestTerms GT GTE NumAbsolute NumDecPlaces NumRelative NumSigFigs PartFrac RegExp SameType SigFigsStrict SingleFrac StringEquiv StringSloppy SubstEquiv SysEquiv UnitsAbsolute UnitsRelative Units UnitsStrictAbsolute UnitsStrictRelative UnitsStrict
function convert(::Type{AbstractString}, x::StackAnswerTest)
  Dict(
    AlgebraicEquivalence => "AlgEquiv",
    CasEqual => "CasEqual",
    CompSquare => "CompSquare",
    Diff => "Diff",
    EqualComAss => "EqualComAss",
    Expanded => "Expanded",
    FacForm => "FacForm",
    IntEquiv => "Int",
    LowestTerms => "LowestTerms",
    GT => "GT",
    GTE => "GTE",
    NumAbsolute => "NumAbsolute",
    NumDecPlaces => "NumDecPlaces",
    NumRelative => "NumRelative",
    NumSigFigs => "NumSigFigs",
    PartFrac => "PartFrac",
    RegExp => "RegExp",
    SameType => "SameType",
    SigFigsStrict => "SigFigsStrict",
    SingleFrac => "SingleFrac",
    StringEquiv => "String",
    StringSloppy => "StringSloppy",
    SubstEquiv => "SubstEquiv",
    SysEquiv => "SysEquiv",
    UnitsAbsolute => "UnitsAbsolute",
    UnitsRelative => "UnitsRelative",
    Units => "Units",
    UnitsStrictAbsolute => "UnitsStrictAbsolute",
    UnitsStrictRelative => "UnitsStrictRelative",
    UnitsStrict => "UnitsStrict",
  )[x]
end

type MoodleFile
  Name::AbstractString
  Path::AbstractString
  Data::Vector{UInt8}
end

"""
    MoodleFile(filename)

generates a file which can be embeded into a `MoodleText`
"""
function MoodleFile(filename::AbstractString)
  if isfile(filename)
    return MoodleFile(basename(filename), "/", read(filename));
  else
    error("File ", filename, " does not exist.");
  end
end

function MoodleFile(name, io::Base.IOBuffer)
  return MoodleFile(name, "/", read(io));
end

function MoodleFile(plot::Any)
  if mimewritable("image/png",plot)
    #io = IOBuffer();
    #print(io,MIME("image/png"),plot);
    #data = takebuf_array(io)
    data = reprmime(MIME("image/png"),plot);
    return MoodleFile("$(uuid1()).png","/",data);
  else
    error("The object does not support printing to a png image.");
  end
end

"""
    EmbedFile(mf::MoodleFile,width="100%",height="100%")

Creates the neccessary code to embed a file in to a `MoodleText`. Currently only works for HTML format.
"""
EmbedFile(mf::MoodleFile;width="100%",height="100%") =
  string("<img src=\"@@PLUGINFILE@@/",mf.Name,"\" alt=\"\" role=\"presentation\" style=\"width: $width; height: $height; vertical-align:text-bottom; margin: 0 .5em;\" class=\"img-responsive\">")

"""
    MoodleText(string,format=HTML)

generates a text, which can be used for all text fields of a `Question` or `Answer`.
"""
type MoodleText
  Text::AbstractString
  Format::MoodleTextFormat
  Files::Vector{MoodleFile}
  MoodleText(Text::AbstractString, Format::MoodleTextFormat, Files::Any) = new(Text,Format,Files)
  MoodleText(Text::AbstractString, Format::MoodleTextFormat) = new(Text,Format, Vector{MoodleFile}())
end

convert(::Type{MoodleText},text::AbstractString) = MoodleText(text,HTML,[])

@enum StackInputType AlgebraicInput CheckboxInput DropdownInput CharacterInput MatrixInput RadioButtonInput TextBoxInput UnitsInput TrueFalseInput
type StackInput
  Type::StackInputType
  Name::AbstractString
  TrueAnswer::AbstractString
  BoxSize::Int
  StrictSyntax::Int
  InsertStars::Int
  SyntaxHint::AbstractString
  SyntaxAttribute::Int
  ForbidWords::AbstractString
  AllowWords::AbstractString
  ForbidFloat::Int
  RequireLowestTerms::Int
  CheckAnswerType::Int
  MustVerify::Int
  ShowValidation::Int
  Options::AbstractString
end

"""
    StackInput(type; optional arguments)

Contstructor for StackInput type using named parameters.
# Arguments
* `Type::StackInputType`           : The type of the input
* `Name::AbstractString`           : The Name of the input, used to embed the Input
* `TrueAnswer::AbstractString`     : A true answer, shown if the student guesses a wrong answer
* `BoxSize::Int=100`               : The width of the input box
* `StrictSyntax::Int=1`            : Whether strict Maxima syntax should be enforced
* `InsertStars::Int=0`             : Whether to insert stars for multiplication
* `SyntaxHint::AbstractString=""`  : A hint that is shown when the box is left empty
* `SyntaxAttribute::Int=0`         : 0 means Syntax hint is editable; 1 means Syntax hint is a placeholder
* `ForbidWords::AbstractString=""` : Words that are forbidden in the answer (comma separated)
* `AllowWords::AbstractString=""`  : Words that are allowed in the answer (comma separated)
* `ForbidFloat::Int=1`             : Whether Floats are forbidden
* `RequireLowestTerms::Int=0`      : Whether Fractions need to be fully cancelled
* `CheckAnswerType::Int=0`         : Whether to check the type of the answer (Equation, Matrix, List, …)
* `MustVerify::Int=1`              : Whether the student needs to verify that Maxima understood their solution
* `ShowValidation::Int=1`          : 0: don‘t show, 1: Show, 2: Show without variable list
* `Options::AbstractString=""`     : Maths formatting options – Not yet implemented
"""
function StackInput(Type::StackInputType, Name::AbstractString, TrueAnswer::AbstractString; BoxSize::Int=15, StrictSyntax::Int=1, InsertStars::Int=0, SyntaxHint::AbstractString="", SyntaxAttribute::Int=0, ForbidWords::AbstractString="", AllowWords::AbstractString="", ForbidFloat::Int=1, RequireLowestTerms::Int=0, CheckAnswerType::Int=0, MustVerify::Int=1, ShowValidation::Int=1, Options::AbstractString="")
  return StackInput(Type, Name, TrueAnswer, BoxSize, StrictSyntax, InsertStars, SyntaxHint, SyntaxAttribute, ForbidWords, AllowWords, ForbidFloat, RequireLowestTerms, CheckAnswerType, MustVerify, ShowValidation, Options)
end

function convert(::Type{AbstractString}, x::StackInputType)
  return Dict(
    AlgebraicInput => "algebraic",
  )[x]
end

function EmbedInput(x::StackInput)
  return string("[[input:", x.Name, "]] [[validation:", x.Name, "]]")
end

type Answer
  Fraction::AbstractFloat
  Text::MoodleText
  Feedback::MoodleText
end

type PRTNode
  Tree::Any
  Name::AbstractString
  AnswerTest::StackAnswerTest
  EvaluatedInput::StackInput
  Answer::AbstractString
  TrueScore::Float32
  FalseScore::Float32
  TrueNextNode::Nullable{PRTNode}
  FalseNextNode::Nullable{PRTNode}
  TrueFeedback::MoodleText
  FalseFeedback::MoodleText
  TrueScoreMode::AbstractString
  FalseScoreMode::AbstractString
end

"""
  PRTNode(EvaluatedInput, Answer; optional arguments)

Constructor for a Potential Response Tree Node, used by stack questions
# Arguments
* `EvaluatedInput::AbstractString`                      : The `StackInput` that is evaluated
* `Answer::AbstractString`                              : The answer that the Input is compared against
* `Name::AbstractString=""`                             : The Name of the Node
* `AnswerTest::StackAnswerTest=AlgebraicEquivalence`    : The method of comparing the Input against the correct Answer
* `TrueScore::Float32=1.0`                              : The question score if this node evaluates to `true`
* `FalseScore::Float32=0.0`                             : The question score if this node evaluates to `frue`
* `TrueNextNode::Nullable{PRTNode}=NULL`                : The next node if this node evaluates to `true`, null if the tree should terminate
* `FalseNextNode::Nullable{PRTNode}=NULL`               : The next node if this node evaluates to `false`, null if the tree should terminate
* `TrueFeedback::MoodleText=""`                         : Additional Feedback if this node evaluates to `true`
* `FalseFeedback::MoodleText=""`                        : Additional Feedback if this node evaluates to `false`
* `TrueScoreMode::AbstractString="="`                   : How the total score is modified if `true`: "=" sets the score to TrueScore, "+" adds to the score, "-" subtracts
* `FalseScoreMode::AbstractString="="`                  : How the total score is modified if `true`: "=" sets the score to TrueScore, "+" adds to the score, "-" subtracts
"""
function PRTNode(Tree, EvaluatedInput, Answer; Name="", AnswerTest=AlgebraicEquivalence, TrueScore=1.0, FalseScore=0.0, TrueNextNode=Nullable{PRTNode}(), FalseNextNode=Nullable{PRTNode}(), TrueFeedback=MoodleText(""), FalseFeedback=MoodleText(""), TrueScoreMode="=", FalseScoreMode="=")
  this = PRTNode(Tree, Name, AnswerTest, EvaluatedInput, Answer, TrueScore, FalseScore, TrueNextNode, FalseNextNode, TrueFeedback, FalseFeedback, TrueScoreMode, FalseScoreMode)
  push!(Tree.Nodes, this)
  return this
end

type PRTree
  Nodes::Vector{PRTNode}
  Name::AbstractString
  Value::Float32
  AutoSimplify::Int
end

"""
  PRTree(Nodes; optional arguments)

Constructor for a Potential Response Tree, used by stack questions
# Arguments
* `Nodes::Vector{PRTNode}` : Nodes of the tree
* `Name::AbstractString="prt1"`          : Name of the tree
* `Value::Float32=1.0`                   : Value of the tree
* `AutoSimplify::Int=1`                  : Whether entered values should be algebraically simplified
"""
function PRTree(;Nodes=[], Name="prt1", Value=1.0, AutoSimplify=1)
  # Assert all Nodes have distinct names
  # Otherwise enumerate the nodes
  return PRTree(Nodes, Name, Value, AutoSimplify)
end


type DragAndDropOption
 Dropable:: Bool
 DragText:: String
 DropText:: String
 DropPosition:: Nullable{Tuple{Float64,Float64}}
 Group:: Integer
 Image:: Nullable{MoodleFile}
end

function DragAndDropOption(Image:: MoodleFile, DragText:: AbstractString = ""; DropText:: AbstractString = "", DropPosition = Nullable{Tuple{Float64,Float64}}(), Group:: Integer = 1)
 isDropable = !isnull(DropPosition)
 return DragAndDropOption(isDropable,DragText, DropText, DropPosition, Group, Image)
end

type Question
  Qtype::QuestionType
  Name::MoodleText
  Text::MoodleText
  GeneralFeedback::MoodleText
  CorrectFeedback::MoodleText
  PartiallyCorrectFeedback::MoodleText
  IncorrectFeedback::MoodleText
  Penalty::Float32
  DefaultGrade::Int
  Hidden::Int
  Single::Bool
  ShuffleAnswers::Bool
  AnswerNumbering::AbstractString
  Answers::Vector{Answer}
  Inputs::Vector{StackInput}
  ProblemResponseTree::Nullable{PRTree}
  DragAndDropImage:: Nullable{MoodleFile}
  DragAndDropOptions:: Array{DragAndDropOption}
end

"""
    Question(type; optional arguments)
Contstructor for Question type using named parameters
# Arguments
* `type::QuestionType`             : see `QustionType` for possible options
* `Name::MoodleText=""`            : title of the question
* `Text::MoodleText=""`            : text containing the actual question
* `GeneralFeedback::MoodleText=""` : this text is always shown after the question has been answered
* `CorrectFeedback::MoodleText="Die Antwort ist richtig."` : this text is shown after the question has been answered corectly
* `PartiallyCorrectFeedback::MoodleText="Die Antwort ist teilweise richtig."` : this text is shown after the question has been partially correctly answered.
* `IncorrectFeedback::MoodleText="Die Antwort ist falsch."` : this text is shown if the question has been answered incorrectly
* `DefaultGrade::Int=1`            : not sure ... probably points awarded if the question is answered correctly in the first attempt
* `Penalty::Float32=1/3`           : not sure ... probably the points awarded for a correct answer is calculated by `DefaultGrade * Penalty` if the student failed to provide the correct answer in the first attempt
* `Hidden::Int=0`                  : not sure ... maybe hidden questions cannot be seen by students?
* `Single::Bool=true`              : not sure ... not idea, by default false for WeightedMultipleChoice
* `ShuffleAnswers::Bool=true`      : decides if answers of questions will be randomly shuffled by Moodle
* `AnswerNumbering::String`="none" : decides how answers of questions should be labeled, e.g. 1. 2. ... or a),b) ... Labels are disabled by default.
* `Answers::Answer=[]`             : `Answer`s for this question
"""
function Question(qtype::QuestionType; Name="", Text="",GeneralFeedback="",CorrectFeedback="Die Antwort ist richtig.",PartiallyCorrectFeedback="Die Antwort ist teilweise richtig.",
 IncorrectFeedback="Die Antwort ist falsch.",Penalty=1/3,DefaultGrade=1,Hidden=0,
 Single=(qtype in [WeightedMultipleChoice]) ? false : true,
 ShuffleAnswers=true,AnswerNumbering="none",Answers=[],Inputs=[], ProblemResponseTree=Nullable{PRTree}(), DragAndDropImage = Nullable{MoodleFile}(), DragAndDropOptions:: Array{DragAndDropOption}=DragAndDropOption[])
  return Question(qtype,Name,Text,GeneralFeedback,CorrectFeedback,PartiallyCorrectFeedback,IncorrectFeedback,Penalty,DefaultGrade,Hidden,Single,ShuffleAnswers,AnswerNumbering,Answers,Inputs,ProblemResponseTree,DragAndDropImage, DragAndDropOptions)
end


"""
    Answer(text; optional arguments)

Contstructor for Question type using named parameters
# Arguments
* `Text::MoodleText=""`     : text containing the actual question
* `Correct::Int=1`          : shortcut for setting wether this answer is correct or not
* `Fraction::Float=100.0`       : `Fraction * Correct /100 * (DefaultGrade of Question)` Points are awarded to the student if this answer is chosen
* `Feedback::MoodleText=""` : this text is always shown after the question has been answered
"""
function Answer(Text;Fraction=100.0,Correct=1,Feedback="")
  if typeof(Fraction) <: Int
    Fraction = Float64(Fraction)
  end
  return Answer( min(Fraction * Correct, Fraction),Text,Feedback)
end

"""
    TrueFalseAnswer(TrueFeedback="",FalseFeedback="",TrueIsCorrect=1)

Shortcut for creating an `Array` with the two `Answer`s "true" and "false". This `Array` can be directly used for the `Answers` argument of `Question`.
"""
function TrueFalseAnswer(TrueFeedback="",FalseFeedback="",TrueIsCorrect=1)
  return [
    Answer(MoodleText("true",MoodleAutoFormat), Correct=TrueIsCorrect, Feedback=TrueFeedback),
    Answer(MoodleText("false",MoodleAutoFormat), Correct=mod(TrueIsCorrect+1,2), Feedback=FalseFeedback)
    ];
end

type EmbeddedAnswerOption
  Text::AbstractString
  Fraction::AbstractFloat
  Feedback::AbstractString
end
"""
    EmbeddedAnswerOption(text; optional arguments)

Contstructor for an embedded answer option using named parameters
# Arguments
* `Text::AbstractString=""`     : text containing the answert option
* `Correct::Int=1`              : shortcut for setting whether this answer option is correct or not
* `Fraction::Float=100`           : `Fraction * Correct /100 * (DefaultGrade of Question)` Points are awarded to the student if this answer is chosen
* `Feedback::AbstractString=""` : text shown to the user has chosen this answer option
"""
function EmbeddedAnswerOption(Text;Correct=1,Fraction=100.0,Feedback="")
  if typeof(Fraction) <: Int
    Fraction = Float64(Fraction)
   end
    return EmbeddedAnswerOption(Text,Correct*Fraction,Feedback);
end

type EmbeddedAnswer
    Type::EmbeddedAnswerType
    Grade::Int
    AnswerOptions::Vector{EmbeddedAnswerOption}
end

"""
    EmbeddedAnswer(type; optional arguments)

Contstructor for EmbeddedAnswer type using named parameters
# Arguments
* `Type::EmbeddedAnswerType=""`                    : type of this answer field
* `Grade::Int=1`                                 : weight of this answer
* `AnswerOptions::Vector{EmbeddedAnswerOption}=[]` : available options for this inline answer
"""
function EmbeddedAnswer(Type;Grade=1,AnswerOptions=[])
    return EmbeddedAnswer(Type,Grade,AnswerOptions);
end





"""
    roundedStringLength(x,AbsTol)
rounds x according to the absolute tolerance AbsTol and returns the length of the resulting string
"""
function roundedStringLength(x::Real,AbsTol::Real)
  return round(x,AbsTol |> log10 |> ceil |> abs |> Int) |> string |> length
end

"""
    NumericalEmbeddedAnswer(value; optional arguments)

Shortcut for constructing an EmbeddedAnswer for a numerical value using named parameters
# Arguments
* `Value`                        : correct value for this question
* `Tolerance`                    : all x with Value - Toleance <= x <= Value + Tolerance will be considered to be correct answers
* `Grade::Int=1`                 : weight of this answer
* `Feedback::AbstractString=""`  : feedback shown if the corret answer is given
* `InputSize::Int`             : minimum size of input element
"""
function NumericalEmbeddedAnswer(Value;Grade=1,Tolerance=0.1,Feedback="",InputSize=0)
  answers = []
  val = Value;
  if isa(Value,Rational)
    # Moodle Version < 1.8 : rational numbers need two seperate answers, the fraction and the float value
    # Moodle Version => 1.8: rational numbers in numerical answers are not supported
    #if den(Value) != 1
    #  push!(answers,EmbeddedAnswerOption(string(num(Value),"/",den(Value));Feedback=Feedback))
    #end
    val = float(Value)

  end
  if !isnan(val) && !isinf(val)
	  # if the toleance settings permit it, we round Value
	  # so that its length is smaller than the size of the input field
	  if val |> string |> length > InputSize
		if roundedStringLength(val,Tolerance)  <=  InputSize
		  val = round(val,Tolerance |> log10 |> ceil |> abs |> Int)
		end
	  end
	  push!(answers,EmbeddedAnswerOption(string(val,":",Tolerance);Feedback=Feedback))
	  # we can set a minimum size s for the input size
	  # by adding a false answer with s digits
	  if InputSize > 0
		k = 1;
		if Value == 10^(InputSize - 1)
		  k = 2;
		end
		push!(answers,EmbeddedAnswerOption(string(k * 10^(InputSize-1),":",Tolerance);Correct=0))
	  end
	  return EmbeddedAnswer(NumericalAnswer,Grade,answers);
   else
      if isnan(val)
	   push!(answers,EmbeddedAnswerOption("nan";Feedback=Feedback));
     else
		  push!(answers,EmbeddedAnswerOption(( (val<0)?"-":"" )*"Inf";Feedback=Feedback));
      # Note: Moodle changes box size to longest answer, which might be revealing - so in case longer answers are allowed, one must adapt the dummy numerical answer,
      # which makes "all" boxes in a matrix unpleasently bigger...
		  #push!(answers,EmbeddedAnswerOption(( (val<0)?"-":"" )*"unendlich";Feedback=Feedback));
		  #push!(answers,EmbeddedAnswerOption(( (val<0)?"-":"" )*"infinity";Feedback=Feedback));
		  #push!(answers,EmbeddedAnswerOption(( (val<0)?"-":"" )*"\\infty";Feedback=Feedback));
		  #push!(answers,EmbeddedAnswerOption(( (val<0)?"-":"" )*"\\infinity";Feedback=Feedback));
	  end
	  return EmbeddedAnswer(ShortAnswerCaseInsensitive,Grade,answers);
   end


end

"""
TrueFalseEmbeddedAnswer(value; optional arguments)

Shortcut for constructing an EmbeddedAnswer for a numerical value using named parameters
# Arguments
* `Value::Bool`                                         : correct value for this question (true or false)
* `Feedback::Array{AbstractString}|AbstractString=""`   : Array of Feedbacks if respective
                                                          (1: choice (!) "true", 2: choice "false") choice is made;
                                                          if only one String, in both cases same Feedbasck
* `AnsNames::Array{AbstractString}=["True","False"]`    : answers to be shown in selection
"""
function TrueFalseEmbeddedAnswer(Value::Bool; Grade=1,Feedback="",AnsNames=["True","False"])
  answers = []

  # if Feedback is not an array, always give the same Feedback to both true and false answer
  if  isa(Feedback,AbstractString)
    Feedback =  [Feedback, Feedback]
  end

  push!(answers,EmbeddedAnswerOption(AnsNames[1];Correct=Int(Value), Feedback=Feedback[1]))
  push!(answers,EmbeddedAnswerOption(AnsNames[2];Correct=Int(!Value), Feedback=Feedback[2]))
  return EmbeddedAnswer(MultipleChoiceHorizontal,Grade,answers);
end

type MatrixEmbeddedAnswer
    A::AbstractMatrix
    Grade::Int
    Tolerance::Float64
    Feedback::AbstractString
    InputSize::Int
    Name::AbstractString
end

"""
    MatrixEmbeddedAnswer(A; optional arguments)

Constructor for MatrixEmbeddedAnswer type using named parameters
# Arguments
* `A::AbstractMatrix`                 : answer matrix
* `Grade::Int=1`                      : weight of this answer
* `Tolerance`                         : all x with Value - Toleance <= x <= Value + Tolerance will be considered to be correct answers
* `Feedback::AbstractString=""`       : feedback shown if the correct answer is given
* `InputSize::Int`                    : (optional) minimum size of input element; if none is set, the matrix elements are rounded according to the tolerance and the length of the longest resulting number is used
* `Name::AbstractString`              : name of the matrix, this adds "name = " infront of the matrix input
"""
function MatrixEmbeddedAnswer(A;Grade=1,Tolerance=0.1,Feedback="",InputSize=0,Name="")
  is = InputSize;
  if InputSize == 0
    is = map( x -> roundedStringLength(x,Tolerance),  A[:]) |> maximum
  end
  return MatrixEmbeddedAnswer(A,Grade,Tolerance,Feedback,is,Name);
end



convert(::Type{AbstractString},ia::EmbeddedAnswer) = string('{',ia.Grade,":",convert(AbstractString,ia.Type),":",convert(AbstractString,ia.AnswerOptions),'}')

convert(::Type{AbstractString},iao::EmbeddedAnswerOption) = (
  string( "%",round(Int64,iao.Fraction),"%", iao.Text, "#", iao.Feedback)
)

convert(::Type{AbstractString},iaov::Vector{EmbeddedAnswerOption}) = (
  join([convert(AbstractString,iao) for iao in iaov],"~")
)

function convert(::Type{AbstractString},mea::MatrixEmbeddedAnswer)
  (m,n) = size(mea.A);
  noBorderStyle = "border: 0px;"
  bracketStyle = "style=\"line-height:100%; font-size:$(2*m+1)em; vertical-align:top; height:100%; padding:0; $(noBorderStyle)\""
  res = "<table style=\"width:auto; vertical-align:middle; display:inline-block; $(noBorderStyle)\">\n"
  for i=1:m
    res = string(res,"<tr>\n")
    padding = "style=\"padding: 0 0 0 0; $(noBorderStyle)\""
    if i==1
        res = string(res,"<td $bracketStyle rowspan=\"$m\">(</td>")
        padding="style=\"padding: 10px 0 0 0; $(noBorderStyle)\""
    end
    for j=1:n
        res = string(res,"<td $padding>",NumericalEmbeddedAnswer(mea.A[i,j],Grade=mea.Grade,Tolerance=mea.Tolerance,Feedback=mea.Feedback,InputSize=mea.InputSize),"</td>")
    end
    if i==1
        res = string(res,"<td $bracketStyle rowspan=\"$m\">)</td>")
    end
    res = string(res,"\n</tr>\n")
  end
  res = string(res,"</table>\n")
  # add "Name = " infront of the matrix
  if mea.Name != ""
    res = string("<span style=\"white-space: nowrap;\">\$",mea.Name,"\$ = ",res,"</span>")
  end
  return res
end

print(ia::EmbeddedAnswer) = convert(AbstractString,ia)
show(io::IO,ia::EmbeddedAnswer) = print(io,convert(AbstractString,ia))

print(mea::MatrixEmbeddedAnswer) = convert(AbstractString,mea)
show(io::IO,mea::MatrixEmbeddedAnswer) = print(io,convert(AbstractString,mea))

"""
    Quiz(Questions::Vector{Question};Category::AbstractString="")

Creates a `Quiz` consisting of the supplied `Question`s with the given category.
"""
type Quiz
  Questions::Vector{Question}
  Category::AbstractString
end

function Quiz(Questions;Category="")
  return Quiz(Questions,Category)
end

# create a Moodle XML document representing the quiz
function buildXML(q::Quiz)
  # create document and root node
  xdoc = XMLDocument();
  xroot = create_root(xdoc, "quiz");

  # Category
  if q.Category != ""
    qnode = new_child(xroot,"question");
    set_attributes(qnode,Dict{Any,AbstractString}("type" => "category"));
    cnode = new_child(qnode,"category");
    tnode = new_child(cnode,"text");
    add_text(tnode,string("\$course\$/", q.Category ));
  end

  # append questions
  for q in q.Questions
    appendXML(q,xroot,xdoc);
  end
  return xdoc
end

# create a Moodle XML document from list of quizes of different categories
function buildXML(qvector::Vector{Quiz})
  # create document and root node
  xdoc = XMLDocument();
  xroot = create_root(xdoc, "quiz");

  # no category quizes at start
  for q in sort(qvector, by = x -> x.Category)
  # Category
    if q.Category != ""
      qnode = new_child(xroot,"question");
      set_attributes(qnode,Dict{Any,AbstractString}("type" => "category"));
      cnode = new_child(qnode,"category");
      tnode = new_child(cnode,"text");
      add_text(tnode,string("\$course\$/", q.Category ));
    end

    # append questions
    for q in q.Questions
      appendXML(q,xroot,xdoc);
    end
  end
  return xdoc
end

"""
    exportXML(quiz)

returns a string with the quiz in Moodle XML format
a vector of  enables to assign different categories
"""
# store the build XML document in a string of file
function exportXML(q::Union{Quiz,Vector{Quiz}})
  s = string(buildXML(q))
end

"""
    exportXML(quiz,filename::AbstractString)

writes the quiz in Moodle XML format to the given file
a vector of  enables to assign different categories
"""
function exportXML(q::Union{Quiz,Vector{Quiz}},filename)
  save_file(buildXML(q),filename);
end

# Append a question to a XML document
function appendXML(q::Question,node,doc)
  # add a question node
  question = new_child(node,"question");
  set_attributes(question,Dict{Any,AbstractString}("type" => q.Qtype));
  # append basic options
  appendXML(q.Name,question,"name",doc);
  appendXML(q.Text,question,"questiontext",doc);
  appendXML(q.Penalty,question,"penalty",doc);
  appendXML(q.DefaultGrade,question,"defaultgrade",doc);
  appendXML(q.Hidden,question,"hidden",doc);
  if q.Qtype != Stack
    appendXML(q.Single,question,"single",doc);
    appendXML(q.ShuffleAnswers,question,"shuffleanswers",doc);
    appendXML(q.AnswerNumbering,question,"answernumbering",doc);
    new_child(question,"shownumcorrect");
    appendXML(q.CorrectFeedback,question,"correctfeedback",doc);
    appendXML(q.PartiallyCorrectFeedback,question,"partiallycorrectfeedback",doc);
    appendXML(q.IncorrectFeedback,question,"incorrectfeedback",doc);
  else
    appendXML(q.CorrectFeedback,question,"prtcorrect",doc)
    appendXML(q.PartiallyCorrectFeedback,question,"prtpartiallycorrect",doc)
    appendXML(q.IncorrectFeedback,question,"prtincorrect",doc)
  end
  appendXML(q.GeneralFeedback,question,"generalfeedback",doc);

  if q.Qtype == TrueFalse
    appendXML(Answer(MoodleText("true",MoodleAutoFormat)),question,doc);
    appendXML(Answer(MoodleText("false",MoodleAutoFormat)),question,doc);
  end
  # append Answers
  if q.Qtype != EmbeddedAnswers && q.Qtype != Stack  && q.Qtype != DragAndDrop
    for answer in q.Answers
      appendXML(answer,question,doc);
    end
  end
  if q.Qtype == Stack
    appendXML(MoodleText(""), question, "questionvariables", doc)
    appendXML(MoodleText(""), question, "questionnote", doc)
    for i in q.Inputs
      appendXML(i, question, doc)
    end
    if !isnull(q.ProblemResponseTree)
      appendXML(MoodleText(
        "[[feedback:$(q.ProblemResponseTree.value.Name)]]"), question, "specificfeedback", doc)
      appendXML(q.ProblemResponseTree.value, question, doc)
    end
  end
  if q.Qtype == DragAndDrop
     if !isnull(q.DragAndDropImage)
       insertXML(get(q.DragAndDropImage), question, doc);
     end
     for (i,o) in enumerate(q.DragAndDropOptions)
       appendXML(o,i,question,doc);
     end

  end
end

# Append an answer to a XML document
function appendXML(a::Answer,node,doc)
  # add an answer node
  answer = new_child(node,"answer");
  set_attributes(answer,Dict{Any,Any}("fraction" => a.Fraction));
  insertXML(a.Text,answer,doc);
  appendXML(a.Feedback,answer,"feedback",doc);
end

# Append an DragAndDropGroup to a XML document
function appendXML(o::DragAndDropOption, optionnumber:: Integer, node,doc)
    drag  = new_child(node,"drag");
    text = new_child(drag,"text");
    add_text(text,o.DragText);
    no = new_child(drag,"no");
    add_text(no,string(optionnumber));
    draggroup = new_child(drag,"draggroup");
    add_text(draggroup, string(o.Group));

    if !isnull(o.Image)
       insertXML(get(o.Image), drag, doc)
    end

    if o.Dropable
       drop  = new_child(node,"drop");
       pos:: Tuple{Float64,Float64} = get(o.DropPosition)
       text = new_child(drop,"text");
       add_text(text,o.DropText);
       no = new_child(drop,"no");
       add_text(no,string(optionnumber));
       choice = new_child(drop,"choice");
       add_text(choice,string(optionnumber));
       xleft = new_child(drop,"xleft");
       add_text(xleft,string(pos[1]));
       ytop = new_child(drop,"ytop");
       add_text(ytop,string(pos[2]));

    end

end


# Append a Moodle text to a XML document
function appendXML(t::MoodleText,node,TagName::AbstractString,doc)
  # create node
  child = new_child(node,TagName);
  insertXML(t,child,doc);
end

# insert a Moodle File into a given node
function insertXML(file::MoodleFile,node,doc)
  child = new_child(node,"file");
  set_attributes(child,Dict{Any,AbstractString}("name" => file.Name,"path" => file.Path,"encoding" => "base64"));
  add_text(child,base64encode(file.Data));
end

# Insert a Moodle text into a given node
function insertXML(t::MoodleText,node,doc)
  # set the format of the text and add the actual text as a new element
  set_attributes(node,Dict{Any,AbstractString}("format" => t.Format));
  text = new_child(node,"text");
  if t.Format == HTML
    add_cdata(doc,text,t.Text);
    # add embedded files
    for file in t.Files
       insertXML(file,node,doc)
    end
  else
    add_text(text,t.Text);
  end
end

# Append a String Option to a XML document
function appendXML(str::AbstractString,node,TagName::AbstractString,doc)
  # create node and add the String as Text
  child = new_child(node,TagName);
  add_text(child,str);
end

# Append an Integer Option to a XML document
function appendXML(n::Int,node,TagName::AbstractString,doc)
  appendXML(string(n),node,TagName,doc)
end

# Append an Float Option to a XML document
function appendXML(n::Float32,node,TagName::AbstractString,doc)
  appendXML(string(n),node,TagName,doc)
end

# Append an Bool Option to a XML document
function appendXML(x::Bool,node,TagName::AbstractString,doc)
  if x
    appendXML("true",node,TagName,doc)
  else
    appendXML("false",node,TagName,doc)
  end
end

function appendXML(input::StackInput, node, doc)
  input_node = new_child(node, "input")
  appendXML(input.Name, input_node, "name", doc)
  appendXML(AbstractString(input.Type), input_node, "type", doc)
  appendXML(input.TrueAnswer, input_node, "tans", doc)
  appendXML(input.BoxSize, input_node, "boxsize", doc)
  appendXML(input.StrictSyntax, input_node, "strictsyntax", doc)
  appendXML(input.InsertStars, input_node, "insertstars", doc)
  appendXML(input.SyntaxHint, input_node, "syntaxhint", doc)
  appendXML(input.SyntaxAttribute, input_node, "syntaxattribute", doc)
  appendXML(input.ForbidWords, input_node, "forbidwords", doc)
  appendXML(input.AllowWords, input_node, "allowwords", doc)
  appendXML(input.ForbidFloat, input_node, "forbidfloat", doc)
  appendXML(input.RequireLowestTerms, input_node, "requirelowestterms", doc)
  appendXML(input.CheckAnswerType, input_node, "checkanswertype", doc)
  appendXML(input.MustVerify, input_node, "mustverify", doc)
  appendXML(input.ShowValidation, input_node, "showvalidation", doc)
  appendXML(input.Options, input_node, "options", doc)
end

function appendXML(prtnode::PRTNode, node, doc)
  xml = new_child(node, "node")
  appendXML(prtnode.Name, xml, "name", doc)
  appendXML(AbstractString(prtnode.AnswerTest), xml, "answertest", doc)
  appendXML(prtnode.EvaluatedInput.Name, xml, "sans", doc)
  appendXML(prtnode.Answer, xml, "tans", doc)

  appendXML(0, xml, "quiet", doc)
  appendXML(prtnode.TrueScoreMode, xml, "truescoremode", doc)
  appendXML(prtnode.TrueScore, xml, "truescore", doc)
  if isnull(prtnode.TrueNextNode)
    appendXML(-1, xml, "truenextnode", doc)
    appendXML(string(prtnode.Tree.Name, "-1-T"), xml, "trueanswernote", doc)
  else
    appendXML(prtnode.TrueNextNode.value.Name, xml, "truenextnode", doc)
    appendXML("", xml, "trueanswernote", doc)
  end
  appendXML(prtnode.TrueFeedback, xml, "truefeedback", doc)

  appendXML(prtnode.FalseScoreMode, xml, "falsescoremode", doc)
  appendXML(prtnode.FalseScore, xml, "falsescore", doc)
  if isnull(prtnode.FalseNextNode)
    appendXML(-1, xml, "falsenextnode", doc)
    appendXML(string(prtnode.Tree.Name, "-1-F"), xml, "falseanswernote", doc)
  else
    appendXML(prtnode.FalseNextNode.value.Name, xml, "falsenextnode", doc)
    appendXML(string(""), xml, "falseanswernote", doc)
  end
  appendXML(prtnode.FalseFeedback, xml, "falsefeedback", doc)
end

function appendXML(prt::PRTree, node, doc)
  # Assert all Nodes have distinct names
  # Otherwise enumerate the nodes
  Names = Set(n.Name for n in prt.Nodes)
  if ("" in Names) | (length(Names) != length(prt.Nodes))
    for (i, n) in enumerate(prt.Nodes)
      n.Name = string(i)
    end
  end
  tree_node = new_child(node, "prt")
  # Append Tree Information
  appendXML(prt.Name, tree_node, "name", doc)
  appendXML(prt.Value, tree_node, "value", doc)
  appendXML(prt.AutoSimplify, tree_node, "autosimplify", doc)
  appendXML(MoodleText(""), tree_node, "feedbackvariables", doc)
  for prtnode in prt.Nodes
    appendXML(prtnode, tree_node, doc)
  end
end

" string input for avoiding the need to escape \\ "
macro M_str(s, flags...) string(s) end
macro M_mstr(s, flags...) string(s) end

end # module
