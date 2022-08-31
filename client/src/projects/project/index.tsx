import { useCallback, useEffect, useState } from 'react'
import { useNavigate, useParams } from 'react-router-dom'
import styled from 'styled-components'

import api from '../../api'
import Loading from '../../components/loading'
import DetailsTab from './details-tab'
import IssuesTab from './issues-tab'

const Root = styled.div`
  box-sizing: border-box;
  height: 100%;
  width: 100%;
  display: flex;
  flex-flow: column nowrap;
`
const Tabs = styled.div`
  width: 100%;
  height: 20px;
  display: flex;
  flex-flow: row nowrap;
  border-bottom: 1px solid gray;
`
const Tab = styled.div`
  padding: 10px;
  display: flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  :hover {
    background-color: white;
  }
  background-color: ${props => props.isSelected ? 'rgba(255,255,255,0.5)' : ''};
`
const ProjectPageTab = {
  Details: 0,
  Issues: 1
}

const ProjectPageTitle = styled(
  ({ className }) => <div className={className}>Project</div>
)`
  font-size: 20px;
  flex-grow: 1;
`
const DeleteProjectButton = ({ projectId }) => {
  const deleteProject = useCallback(() => {
    (async () => {
      api.deleteApiV1ProjectsDeleteByProjectId(projectId);
      navigate('/projects')
    })()
  }, [projectId]);

  return <button onClick={deleteProject}>Delete Project</button>
}
const HeaderRow = styled(({ className, projectId }) =>
  <div className={className}>
    <ProjectPageTitle />
    <DeleteProjectButton projectId={projectId} />
  </div>
)`
  height: 30px;
  padding: 5px;
  display: flex;
  flex-flow: row nowrap;
`

const View = ({ selectedTab }) => {
  const navigate = useNavigate();
  const { projectId } = useParams();

  const tab = (() => {
    if (selectedTab === ProjectPageTab.Details) {
      return <DetailsTab projectId={projectId} />
    }
    if (selectedTab === ProjectPageTab.Issues) {
      return <IssuesTab projectId={projectId} />
    }
  })()

  return (
    <Root>

      <HeaderRow projectId={projectId} />

      <Tabs>
        <Tab
          isSelected={selectedTab === ProjectPageTab.Details}
          onClick={() => navigate(`/projects/${projectId}`)}
        >
          Project Details
        </Tab>
        <Tab
          isSelected={selectedTab === ProjectPageTab.Issues}
          onClick={() => navigate(`/projects/${projectId}/issues`)}
        >
          Issues
        </Tab>
      </Tabs>

      {tab}

    </Root>
  );
}

export const Project = () => <View selectedTab={ProjectPageTab.Details} />
export const Issues = () => <View selectedTab={ProjectPageTab.Issues} />
