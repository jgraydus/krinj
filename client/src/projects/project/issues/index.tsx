import { useEffect } from 'react'
import styled from 'styled-components'
import { Loading } from 'components'
import { loadEntities, selectEntitiesForProject, useDispatch, useSelector } from 'data'
import Table from './table'

const NoIssues = styled(({ className }) => <div className={className}>No Issues</div>)`
  width: 100%;
  height: 100%;
  font-size: 30px;
  display: flex;
  flex-flow: row nowrap;
  align-items: center;
  justify-content: center;
`
const Content = ({ issues, projectId }: { issues: Array<Entity>, projectId: ProjectId }) => {
  if (issues == null) {
      return <Loading />;
  }
  if (issues.length === 0) {
      return <NoIssues />;
  }
  return <Table issues={issues} projectId={projectId} />;
}

export default ({ projectId }: { projectId: ProjectId }) => {
  const dispatch = useDispatch();
  const issues = useSelector(state => selectEntitiesForProject(state, projectId));

  useEffect(() => { dispatch(loadEntities(projectId)); }, []);

  return <Content issues={issues} projectId={projectId} />
}

