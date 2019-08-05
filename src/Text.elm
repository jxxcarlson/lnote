module Text exposing (manual)


manual =
    """
# Manual

- In case you were wondering: text that you are editing is automatically
saved twice per second (if the text has changed).

- The **Download** button will download
the current selection to a text file
`notes.yaml` which represents the
selection in yaml form.  Thus the notes
you create are not hostage to the system.

- **Key commands:**
  - ^B: Switch to browse mode
  - ^E: Edit the current note
  - ^M: Display (or hide) the manual
  - ^N: New note
  - ^A: Sort alphabetically
  - ^U: Sort by decreasing modification time
  - ^V: Sort by increasing modification time

- **Search.**  Put "intro po" in the "Search by subject"
box to search for all notes containing "intro" and "po"
in the subject, e.g., "Introduction to Poetry" and
"Intro to Political Science." Searching the body of the note
(full text search) works the same way.  Likewise for tags.
"""
