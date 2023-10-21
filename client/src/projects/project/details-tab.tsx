import { useCallback, useEffect, useState } from 'react'
import { EditText, EditTextarea } from 'react-edit-text'
import 'react-edit-text/dist/index.css'
import styled from 'styled-components'

import Loading from '../../components/loading'
import MdEditor from '../../components/md-editor'
import { loadProject, selectProject, useDispatch, updateProject, useSelector } from '../../data'

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
const View = ({ project, saveDescription, saveTitle }: { project: Project, saveDescription: any, saveTitle: any }) =>
  <Root>
    <div>Title</div>
    <EditText
      name="ProjectTitle"
      defaultValue={project.name}
      onSave={saveTitle}
    />

    <div>Description</div>
    <MdEditor
      initialValue={project.description}
      onSave={saveDescription}
    />
  </Root>

export default ({ projectId }: { projectId: ProjectId }) => {
  const dispatch = useDispatch();
  const project = useSelector(state => selectProject(state, projectId));

  const saveTitle = useCallback(
      ({ value }: { value: string }) => {
          dispatch(updateProject(projectId, { projectName: value || 'ProjectTitle' }));
      },
      [projectId]
  )
  const saveDescription = useCallback(
      (value: string) => {
          dispatch(updateProject(projectId, { projectDescription: value || '' }));
      },
      [projectId]
  );

  if (project === null) {
    return <Loading />
  }

  return <View project={project} saveDescription={saveDescription} saveTitle={saveTitle} />
}

