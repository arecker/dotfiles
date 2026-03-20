# Interactive Reading

Read through content item-by-item with progress tracking and conversational engagement.

## Usage

Just say:
- "let's read these tickets"
- "let's read EPIC-123"
- "let's read this document"
- "let's read sprint 7 backlog"

## What It Does

1. **Shows full content** - Complete text of each item like an e-reader
2. **Tracks progress** - Unicode checkboxes (☐/☑) show what you've reviewed
3. **Engages conversationally** - Share your thoughts, get feedback and insights
4. **Navigates naturally** - Say "next", "back", "skip", or jump to specific items
5. **Remembers context** - Your observations stay in conversation, summarizable on request

## Navigation

- `next` - Move to next item
- `skip` / `skip this` - Mark done and advance
- `back` / `previous` - Go back one
- `go to [identifier]` - Jump to specific item
- `done` / `finish reading` - End session with summary

## What You Can Read

- **Jira tickets** - By epic, sprint, or list of keys
- **Documents** - Markdown files, sections, chapters
- **Any sectioned content** - Lists, reports, anything with discrete parts

## Notes

Your thoughts and observations are kept in conversation context. Ask for "scratch notes" or "summary" anytime to see what you've captured.

## Example

```
You: "let's read these tickets"

[Shows progress tracker and first ticket's full content]

You: "This needs acceptance criteria before grooming"

Claude: "Yeah, 'operates as expected' is too vague. Should we define specific tests?"

You: "next"

[Shows next ticket...]
```
