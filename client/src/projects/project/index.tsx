import * as R from 'ramda'
import { useCallback, useEffect } from 'react'
import { useNavigate, useParams } from 'react-router-dom'
import styled from 'styled-components'
import { Button } from 'components'
import DetailsTab from './details-tab'
import IssuesTab from './issues-tab'
import { deleteProject, loadProject, selectProject, useDispatch, useSelector } from 'data'

const Root = styled.div`
  box-sizing: border-box;
  height: 100%;
  width: 100%;
  display: flex;
  flex-flow: column nowrap;
`
const Tabs = styled.div`
  width: 100%;
  height: 22px;
  display: flex;
  flex-flow: row nowrap;
  border-bottom: 1px solid gray;
`
const Tab = styled.div<{ isSelected: boolean }>`
  padding: 10px;
  display: flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  border-radius: 8px 8px 0 0;
  :hover {
    background-color: rgba(255,255,255,0.5);
  }
  border-left: ${props => props.isSelected ? '1px solid gray' : '0'};
  border-top: ${props => props.isSelected ? '1px solid gray' : '0'};
  border-right: ${props => props.isSelected ? '1px solid gray' : '0'};
  border-bottom: ${props => props.isSelected ? '1px solid #BFCFCF' : '0'};
`
const enum ProjectPageTab { Details, Issues }

const ProjectPageTitle = styled(
  ({ className, projectName }) => <div className={className}>Project: {projectName}</div>
)`
  font-size: 20px;
  flex-grow: 1;
`
const DeleteProjectButton = ({ projectId }: { projectId: ProjectId }) => {
  const dispatch = useDispatch();
  const navigate = useNavigate();

  const _deleteProject = useCallback(() => {
    dispatch(deleteProject(projectId));
    navigate('/projects');
  }, [projectId]);

  return <Button onClick={_deleteProject}>Delete Project</Button>
}

const HeaderRow = styled(({ className, projectId, projectName }) =>
  <div className={className}>
    <ProjectPageTitle projectName={projectName} />
    <DeleteProjectButton projectId={projectId} />
  </div>
)`
  height: 40px;
  padding: 5px;
  display: flex;
  flex-flow: row nowrap;
`

const View = ({ selectedTab }: { selectedTab: ProjectPageTab } ) => {
  const dispatch = useDispatch();
  const navigate = useNavigate();
  const { projectId }: any = useParams();
  const project: Project | null = useSelector(state => selectProject(state, projectId));

  useEffect(() => {
    dispatch(loadProject(projectId));
  }, [projectId]);

  if (project === null) {
      return null;
  }

  const tab = (() => {
    if (selectedTab === ProjectPageTab.Details) {
      return <DetailsTab projectId={projectId} />
    }
    if (selectedTab === ProjectPageTab.Issues) {
      return <IssuesTab project={project} />
    }
    return null;
  })()

  return (
    <Root>

      <HeaderRow projectId={projectId} projectName={R.propOr('', 'name', project || {})}/>

      <Tabs>
        <Tab
          isSelected={selectedTab === ProjectPageTab.Issues}
          onClick={() => navigate(`/projects/${projectId}/issues`)}
        >
          Issues
        </Tab>
        <Tab
          isSelected={selectedTab === ProjectPageTab.Details}
          onClick={() => navigate(`/projects/${projectId}`)}
        >
          Project Details
        </Tab>
      </Tabs>

      {tab}

    </Root>
  );
}

export const Project = () => <View selectedTab={ProjectPageTab.Details} />
export const Issues = () => <View selectedTab={ProjectPageTab.Issues} />

