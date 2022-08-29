import { useCallback, useEffect, useState } from 'react'
import { EditText, EditTextarea } from 'react-edit-text'
import 'react-edit-text/dist/index.css'
import styled from 'styled-components'

import api from '../api'
import Loading from '../components/loading'
import MdEditor from '../components/md-editor'

const Root = styled.div`
  box-sizing: border-box;
  height: 100%;
  width: 100%;
  padding: 5px;
  display: grid;
  grid-template-columns: 100px auto;
  grid-template-rows: auto 1fr;
  gap: 5px;

  ._4GdcU, input {
    box-sizing: border-box;
    height: 30px;
    width: 100%;
    display: block;
    margin: 3px 0;
    scrollbar-width: thin;
    border: 1px solid #AAA;
  }
`

const save = ({ projectId, name }) => ({ value, previousValue }) => {
  api.patchApiV1ProjectsByProjectId(projectId, [
    { tag: name, contents: value },
  ])
}

export default ({ projectId }) => {
  const [project, setProject] = useState(null);

  useEffect(() => {
    (async () => {
      const project = await api.getApiV1ProjectsByProjectId(projectId);
      setProject(project);
    })()
  }, [projectId]);

  const saveTitle = useCallback(save({ projectId, name: 'ProjectTitle' }), [projectId])
  const saveDescription = useCallback(value => save({ projectId, name: 'ProjectDescription' })({ value }), [projectId]);

  if (project === null) {
    return <Loading />
  }

  return (
    <Root>
      <div>Title</div>
      <EditText
        name="ProjectTitle"
        defaultValue={project.title}
        onSave={saveTitle}
      />

      <div>Description</div>
      <MdEditor
        initialValue={project.description}
        onSave={saveDescription}
      />
    </Root>
  )
}
