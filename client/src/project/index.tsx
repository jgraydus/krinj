import { useCallback, useEffect, useState } from 'react'
import { EditText, EditTextarea } from 'react-edit-text'
import 'react-edit-text/dist/index.css'
import { useParams } from 'react-router-dom'
import styled from 'styled-components'

import api from '../api'
import Loading from '../components/loading'
import MdEditor from '../components/md-editor'

const Root = styled.div`
  box-sizing: border-box;
  padding: 5px;
  height: 100%;
  width: 100%;
  display: flex;
  flex-flow: column nowrap;
`
const Header = styled.div`
  font-size: 20px;
`
const Table = styled.div`
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

const save = projectId => ({name, value, previousValue}) => {
  if (value !== previousValue) {
    api.patchApiV1ProjectsByProjectId(projectId, [
      { tag: name, contents: value },
    ])
  }
}

const Project = ({ title, description, onSave }) => {
  const saveDescription = useCallback(value => {
      onSave({ name: 'ProjectDescription', value, previousValue: null })
  }, [onSave]);

  return (
    <Root>
      <Header>Project</Header>
      <Table>
         <div>Title</div>
         <EditText
           name="ProjectTitle"
           defaultValue={title}
           onSave={onSave}
         />

         <div>Description</div>
         <MdEditor initialValue={description} onSave={saveDescription}/>
      </Table>
    </Root>
  );
}

export default () => {
  const { projectId } = useParams();
  const [project, setProject] = useState(null);

  useEffect(() => {
    (async () => {
      const project = await api.getApiV1ProjectsByProjectId(projectId);
      setProject(project);
    })()
  }, [projectId]);

  return (
    !!project ? <Project {...project} onSave={save(project.projectId)} /> : <Loading />
  )
}

