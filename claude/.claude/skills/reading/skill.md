# Interactive Reading

Guide the user through reading content item by item with progress tracking and conversational engagement.

## Trigger Phrases

- "let's read these tickets"
- "let's read [EPIC-123]"
- "let's read this document"
- "let's read [file.md]"

## Behavior

### 1. Initialize Reading Session

When triggered, identify what to read:
- **Jira tickets**: By epic, sprint, or list of ticket keys
- **Documents**: Markdown files, Confluence pages, any sectioned content
- **Lists**: Any numbered or sectioned content

Parse the content into discrete items (tickets, sections, chapters, etc.) and display initial progress tracker.

### 2. Display Current Item

Show the **full text** of the current item:
- For tickets: Summary, description, acceptance criteria, comments
- For documents: Full section text with heading
- For other content: Complete item content

Always include the progress tracker showing:
- Total count (e.g., "Progress: 3/9")
- Unicode checkboxes (☐ unchecked, ☑ checked)
- Item identifiers/titles

### 3. Listen and Engage

After displaying content:
- Wait for user thoughts, questions, or observations
- Respond **conversationally** - engage with their thinking, add insights, answer questions
- Keep track of their notes in context (don't write them anywhere)
- DO NOT auto-advance to next item

### 4. Navigation

Move between items only on explicit commands:
- "next" - advance to next item
- "skip" / "skip this" - mark current as done and advance
- "back" / "previous" - go back one item
- "go to [identifier]" - jump to specific item
- Natural references work too: "let's look at the RFC ticket"

When advancing, mark the current item as checked (☑) and display the next item's full text.

### 5. Session Management

Throughout the session:
- Keep user's thoughts and observations in conversation context
- If user asks for "scratch notes" or "summary", provide their key observations
- If user says "done" or "finish reading", show final progress and summarize session
- Natural conversation flow - don't be robotic about it

## Response Style

- **Conversational and warm** - this is collaborative reading, not a quiz
- Comment meaningfully on their observations
- Ask follow-up questions when appropriate
- Share relevant insights about the content
- Keep progress tracker concise but visible

## Example Flow

```
User: "let's read these tickets"
Claude: *Fetches tickets from epic PROJ-100*

**Progress: 0/4**

☐ PROJ-101: Write technical design document
☐ PROJ-102: Implement core API endpoints
☐ PROJ-103: Add authentication layer
☐ PROJ-104: Deploy to staging environment

---

## PROJ-101: Write technical design document

**Reporter:** Jane Smith
**Status:** To Do
**Assignee:** Unassigned

**Description:**
Create a technical design document outlining the architecture for the new API service. Include system diagrams, data flow, and integration points with existing systems.

**Acceptance Criteria:**
• Design document covers all major components
• Review completed by tech leads
• Document published to team wiki

User: "This looks like it should happen last, after we've built everything and know what we're documenting."

Claude: "Good catch. The design doc might be more valuable if written early though - it forces you to think through the architecture before coding. But I see your point about having real implementation details to reference."

User: "Let's see what the other tickets look like first. Next."

Claude: "On it."

**Progress: 1/4**

☑ PROJ-101: Write technical design document
☐ PROJ-102: Implement core API endpoints
...
```

## Notes

- The skill should feel natural and conversational
- Don't force structured output unless the user asks for it
- Keep the progress tracker consistent and visible
- Be ready to summarize the user's thoughts at any time
- Natural language navigation should work seamlessly

## Technical Details

### Jira Ticket Display

When fetching Jira tickets, always include these fields:
- **Reporter**: `fields.reporter.displayName`
- **Status**: `fields.status.name`
- **Assignee**: `fields.assignee.displayName`
- **Description**: `fields.description` (ADF format)
- **Acceptance Criteria**: `customfield_19300` (ADF format)
- **Comments**: `fields.comment.comments` (optional, last few)

The acceptance criteria field is stored in `customfield_19300` and uses Atlassian Document Format (ADF).
