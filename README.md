# MoodleQuiz

This module facilitates the creation of a Moodle quiz. It provides a `Quiz` data structure corresponding to the Moodle XML format for a quiz as well as functions to export this data structure to said Moodle XML Format.

## Example
```julia
# a simple muliplie choice question
q1 = Question(MultipleChoice,
	     Name="Through The Galaxy",
             Text="What is the answer to the Ultimate Question of Life, The Universe, and Everything?",
	     IncorrectFeedback="Read The Hitchhiker's Guide to the Galaxy",
	     Answers=[
		Answer("21",Correct=0),
		Answer("42",Correct=1),
		Answer("0" ,Correct=0)
	     ]
     );

# a true / false question
q2 = Question(TrueFalse,
	     Name="Boldy Go Where ...",
             Text="Does god need spaceship?",
	     IncorrectFeedback="Watch number V",
	     Answers=TrueFalseAnswer()
    );

# question containing latex code (the M" " macro avoids the need to escape $,\,... )
q3  = Question(MultipleChoice,
	       Name="Circle",
	       Text=M"What is the area of a circle with radius $r$?",
	       Answers=[
	          Answer(M"$\pi$"    ,Correct=0),
	          Answer(M"$2 \pi$"  ,Correct=0),
	          Answer(M"$\pi r^2$",Correct=1)
	       ]
      );

# create a quiz and export it
quiz = Quiz([q1, q2, q3]);
exportXML(quiz,"Space.xml")
```

## Currently Supported Quiz Types
* Multiple Choice
* All Or Nothing Multiple Choice
* Simple Yes/No Questions 
