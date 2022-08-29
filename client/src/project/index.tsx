import { useCallback, useEffect, useState } from 'react'
import { EditText, EditTextarea } from 'react-edit-text'
import 'react-edit-text/dist/index.css'
import { useNavigate, useParams } from 'react-router-dom'
import styled from 'styled-components'

import api from '../api'
import Loading from '../components/loading'
import MdEditor from '../components/md-editor'

const Root = styled.div`
  box-sizing: border-box;
  height: 100%;
  width: 100%;
  display: flex;
  flex-flow: column nowrap;
`
const HeaderRow = styled.div`
  height: 30px;
  padding: 5px;
  display: flex;
  flex-flow: row nowrap;
`
const ProjectPageTitle = styled(
  ({ className }) => <div className={className}>Project</div>
)`
  font-size: 20px;
  flex-grow: 1;
`
const ProjectDetails = styled.div`
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
const ProjectIssues = styled.div`
  box-sizing: border-box;
  display: flex;
  flex-flow: column nowrap;
  height: 100%;
  width: 100%;
  padding: 5px;
  border: 1px solid red;
`
const NoIssues = styled(({ className }) => <div className={className}>No Issues</div>)`
  width: 100%;
  height: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 20px;
`
const Table = styled.div`
  box-sizing: border-box;
  display: flex;
  flex-flow: column nowrap;
  width: 100%;
  height: 0px;
  flex-grow: 1;
  flex-shrink: 1;
  border: 1px solid purple;
`

const save = projectId => ({name, value, previousValue}) => {
  if (value !== previousValue) {
    api.patchApiV1ProjectsByProjectId(projectId, [
      { tag: name, contents: value },
    ])
  }
}

const createNewIssue = async projectId => {
  const issue = await api.postApiV1IssuesCreate(JSON.stringify(projectId));
  await api.patchApiV1IssuesByIssueId(issue.issueId, [{
    tag: 'Title', contents: 'ISSUE A'
  }]);
  return issue;
}

const ProjectPageTab = {
  Details: 0,
  Issues: 1
}

const Layout = ({ deleteProject, description, issues, onSave, projectId, selectedTab, setIssues, title }) => {
  const navigate = useNavigate();
  const saveDescription = useCallback(value => {
      onSave({ name: 'ProjectDescription', value, previousValue: null })
  }, [onSave]);

  const newIssue = useCallback(async () => {
    const issue = await createNewIssue(projectId);
    setIssues([iissue, ...issues]);
  }, [projectId]);

  return (
    <Root>
      <HeaderRow>
        <ProjectPageTitle />
        <button onClick={deleteProject}>Delete Project</button>
      </HeaderRow>
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
      {selectedTab === ProjectPageTab.Details ? (
         <ProjectDetails>
           <div>Title</div>
           <EditText
             name="ProjectTitle"
             defaultValue={title}
             onSave={onSave}
           />

           <div>Description</div>
           <MdEditor initialValue={description} onSave={saveDescription}/>
        </ProjectDetails>
      ) : (
        <ProjectIssues>
          <button onClick={newIssue}>New Issue</button>
          {issues.length === 0 ? <NoIssues /> : <Table issues={issues} />}
        </ProjectIssues>
      )}
    </Root>
  );
}

const View = ({ selectedTab }) => {
  const navigate = useNavigate();
  const { projectId } = useParams();
  const [project, setProject] = useState(null);
  const [issues, setIssues] = useState(null);

  useEffect(() => {
    (async () => {
      const project = await api.getApiV1ProjectsByProjectId(projectId);
      const issues = await api.getApiV1Issues(projectId, null);
      setIssues(issues);
      setProject(project);
    })()
  }, [projectId]);

  const deleteProject = useCallback(() => {
    (async () => {
      api.deleteApiV1ProjectsDeleteByProjectId(projectId);
      navigate('/projects')
    })()
  }, [projectId]);

  return (
    !!project ? (
      <Layout 
        deleteProject={deleteProject}
        issues={issues}
        onSave={save(project.projectId)}
        selectedTab={selectedTab}
        setIssues={setIssues}
        {...project}
      />
    ) : (
      <Loading />
    )
  )
}

export const Project = () => <View selectedTab={ProjectPageTab.Details} />
export const Issues = () => <View selectedTab={ProjectPageTab.Issues} />

