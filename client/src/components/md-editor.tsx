import { useState, useCallback } from 'react'
import ReactMarkdown from 'react-markdown'
import remarkGfm from 'remark-gfm'
import styled from 'styled-components'

const Root = styled.div`
  border: 1px solid black;
  height: 100%;
  width: 100%;
  display: flex;
  flex-flow: column nowrap;
`
const Editor = styled.div`
  padding: 5px;
  flex-grow: 1;

  .task-list-item {
    display: flex;
    flex-flow: row wrap;
    align-items: center;
    input {
      display: inline;
      width: 20px;
      height: 20px;
      margin-right: 10px;
    }
  }
`
const Textarea = styled.textarea`
  box-sizing: border-box;
  width: 100%;
  height: 100%;
  resize: none;
  outline: none;
  border: none;
`

enum EditorMode { Write, Preview }

export default ({ onSave, initialValue }: { onSave: (arg: string) => void, initialValue?: string }) => {
    const [markdown, setMarkdown] = useState(initialValue || '');
    const [mode, setMode] = useState(EditorMode.Preview);

    const toggleMode = useCallback(() => {
        if (mode === EditorMode.Preview) {
            setMode(EditorMode.Write);
        } else {
            onSave(markdown);
            setMode(EditorMode.Preview);
        }
    }, [markdown, mode, setMode]);

    return (
      <Root>
        <Editor>
          {mode === EditorMode.Preview ? (
            <ReactMarkdown remarkPlugins={[remarkGfm]}>
              {markdown}
            </ReactMarkdown>
          ) : (
            <Textarea
              value={markdown}
              onChange={e => setMarkdown(e.target.value)}
            />
          )}
        </Editor>
        <button onClick={toggleMode}>
          {mode === EditorMode.Preview ? "Edit" : "Preview (save changes)"}
        </button>
      </Root>
    );
}

