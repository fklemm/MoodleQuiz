using Documenter, MoodleQuiz

makedocs()

deploydocs(
    deps   = Deps.pip("mkdocs", "python-markdown-math"),
    repo   = "github.com/tauu/MoodleQuiz.git",
    julia  = "release",
    osname = "linux"
)
