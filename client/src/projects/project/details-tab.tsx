import { useCallback } from 'react'
import styled from 'styled-components'
import { InlineEdit, Loading, MdEditor } from 'components'
import { selectProject, useDispatch, updateProject, useSelector } from 'data'

const Root = styled.div`
  box-sizing: border-box;
  height: 100%;
  width: 100%;
  padding: 5px;
  display: grid;
  grid-template-columns: 100px auto;
  grid-template-rows: auto 1fr;
  gap: 5px;
`
const Label = styled.div`
  height: 30px;
  display: flex;
  align-items: center;
  justify-content: left;
`
const View = ({ project, saveDescription, saveTitle }: { project: Project, saveDescription: any, saveTitle: any }) =>
  <Root>
    <Label>Title</Label>
    <InlineEdit
      initialValue={project.name}
      onSave={saveTitle}
    />

    <Label>Description</Label>
    <MdEditor
      initialValue={project.description}
      onSave={saveDescription}
    />
  </Root>

export default ({ projectId }: { projectId: ProjectId }) => {
  const dispatch = useDispatch();
  const project = useSelector(state => selectProject(state, projectId));

  const saveTitle = useCallback(
      (value: string) => {
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

