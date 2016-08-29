module MoodleQuiz

using LightXML

import Base.convert

export QuestionType, Question, Answer, MoodleText, MoodleTextFormat, Quiz, TrueFalseAnswer, exportXML, @M_str, @M_mstr
export MultipleChoice, TrueFalse, ShortAnswer, Matching, Cloze, Essay, Numerical, Description, CalculatedSimple, DragAndDrop, DragAndDropMatch, AllOrNothingMultipleChoice

"""
Moodle supports the following types of questions

 Type                        | Description
 :---------------------------| :---------------------------------------------------------------------------------------------------------
 MultipleChoice              | standard multiple choice question
 AllOrNothingMultipleChoice  | multiple choice question, where all correct and no false answers have to be selected to recieve any points
 TrueFalse                   | simple yes / no question
 ShortAnswer                 | not yet implemented
 Matching                    | not yet implemented
 Cloze                       | not yet implemented
 Essay                       | not yet implemented
 Numerical                   | not yet implemented
 Description                 | not yet implemented
 CalculatedSimple            | not yet implemented
 DragAndDrop                 | not yet implemented
 DragAndDropMatch            | not yet implemented
"""
@enum QuestionType MultipleChoice TrueFalse ShortAnswer Matching Cloze Essay Numerical Description CalculatedSimple DragAndDrop DragAndDropMatch AllOrNothingMultipleChoice
convert(::Type{AbstractString},x::QuestionType) = (
   get(Dict(
    # MultipleChoice => "multichoice",
    MultipleChoice => "oumultiresponse",
    AllOrNothingMultipleChoice => "multichoiceset",
    TrueFalse => "truefalse",
    ShortAnswer => "shortanswer",
    Matching => "matching",
    Cloze => "cloze",
    Essay => "essay",
    Numerical => "numerical",
    Description => "description",
    CalculatedSimple => "calculatedsimple",
    DragAndDrop => "ddimageortext",
    DragAndDropMatch => "ddmatch"
  ),x,"")
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
    MoodleText(string,format=HTML)

generates a text, which can be used for all text fields of a `Question` or `Answer`.
"""
type MoodleText
  Text::AbstractString
  Format::MoodleTextFormat
end

convert(::Type{MoodleText},text::AbstractString) = MoodleText(text,HTML)

type Answer
  Fraction::Int
  Text::MoodleText
  Feedback::MoodleText
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
* `Single::Bool=true`              : not sure ... not idea
* `ShuffleAnswers::Bool=true`      : decides if answers of questions will be randomly shuffled by Moodle
* `AnswerNumbering::String`="none" : decides how answers of questions should be labeled, e.g. 1. 2. ... or a),b) ... Labels are disabled by default.
* `Answers::Answer=[]`             : `Answer`s for this question
"""
function Question(qtype::QuestionType; Name="", Text="",GeneralFeedback="",CorrectFeedback="Die Antwort ist richtig.",PartiallyCorrectFeedback="Die Antwort ist teilweise richtig.", IncorrectFeedback="Die Antwort ist falsch.",Penalty=1/3,DefaultGrade=1,Hidden=0,Single=true,ShuffleAnswers=true,AnswerNumbering="none",Answers=[])
  return Question(qtype,Name,Text,GeneralFeedback,CorrectFeedback,PartiallyCorrectFeedback,IncorrectFeedback,Penalty,DefaultGrade,Hidden,Single,ShuffleAnswers,AnswerNumbering,Answers)
end

"""
    Answer(text; optional arguments)

Contstructor for Question type using named parameters
# Arguments
* `Text::MoodleText=""`     : text containing the actual question
* `Correct::Int=1`          : shortcut for setting wether this answer is correct or not
* `Fraction::Int=100`       : `Fraction * Correct /100 * (DefaultGrade of Question)` Points are awarded to the student if this answer is chosen
* `Feedback::MoodleText=""` : this text is always shown after the question has been answered
"""
function Answer(Text;Fraction=100,Correct=1,Feedback="")
  return Answer(Fraction * Correct,Text,Feedback)
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

"""
    exportXML(quiz)

returns a string with the quiz in Moodle XML format
"""
# store the build XML document in a string of file
function exportXML(q::Quiz)
  s = string(buildXML(q))
end

"""
    exportXML(quiz,filename::AbstractString)

writes the quiz in Moodle XML format to the given file
"""
function exportXML(q::Quiz,filename)
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
  appendXML(q.Single,question,"single",doc);
  appendXML(q.ShuffleAnswers,question,"shuffleanswers",doc);
  appendXML(q.AnswerNumbering,question,"answernumbering",doc);
  new_child(question,"shownumcorrect");
  appendXML(q.GeneralFeedback,question,"generalfeedback",doc);
  appendXML(q.CorrectFeedback,question,"correctfeedback",doc);
  appendXML(q.PartiallyCorrectFeedback,question,"partiallycorrectfeedback",doc);
  appendXML(q.IncorrectFeedback,question,"incorrectfeedback",doc);

  if q.Qtype == TrueFalse
    appendXML(Answer(MoodleText("true",MoodleAutoFormat)),question,doc);
    appendXML(Answer(MoodleText("false",MoodleAutoFormat)),question,doc);
  end
  # append Answers
  for answer in q.Answers
    appendXML(answer,question,doc);
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

# Append a Moodle text to a XML document
function appendXML(t::MoodleText,node,TagName::AbstractString,doc)
  # create node
  child = new_child(node,TagName);
  insertXML(t,child,doc);
end

# Insert a Moodle text into a given node
function insertXML(t::MoodleText,node,doc)
  # set the format of the text and add the actual text as a new element
  set_attributes(node,Dict{Any,AbstractString}("format" => t.Format));
  text = new_child(node,"text");
  if t.Format == HTML
    add_cdata(doc,text,t.Text);
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

" string input for avoiding the need to escape \\ "
macro M_str(s, flags...) string(s) end
macro M_mstr(s, flags...) string(s) end

end # module
