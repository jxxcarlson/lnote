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

- **Key commands.** Not all key commands correspond to a button.
  - ^A: Sort alphabetically
  - ^B: Switch to browse mode
  - ^D: Sort by decreasing modification time
  - ^E: Edit the current note
  - ^I: Sort by increasing modification time
  - ^M: Display (or hide) the manual
  - ^N: New note (set it up)
  - ^R: Random selection of notes
  - ^S: Save the new new note
  - ^U: Unselect: show all notes

- **Search.**  Put "intro po" in the "Search by subject"
box to search for all notes containing "intro" and "po"
in the subject, e.g., "Introduction to Poetry" and
"Intro to Political Science." Searching the body of the note
(full text search) works the same way.  Likewise for tags.
"""
