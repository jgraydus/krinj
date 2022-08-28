import { useCallback, useState } from 'react'
import { useNavigate } from 'react-router-dom'
import styled from 'styled-components'
import api from '../api'
import InlineEdit from '../components/inline-edit'
import Modal from '../components/modal'
import Spacer from '../components/spacer'

const Root = styled.div`
  width: 100%;
  height: 100%;
  display: flex;
  flex-flow: column nowrap;

  input {
    width: 100%;
    height: 30px;
    border: 1px solid black;
  }
`
const Title = styled.div`
  font-size: 20px;
`
const Content = styled.div`
  width: 100$;
  height: 0;
  flex-grow: 1;
  flex-strink: 1;
`
const Footer = styled.div`
  display: flex;
  flex-flow: row nowrap;
  justify-content: flex-end;
  align-self: flex-end;
`
const ProjectName = styled.div`
  font-size: 14px;
`
const createProject = async projectName => {
  const { projectId } = await api.postApiV1ProjectsCreate();
  await api.patchApiV1ProjectsByProjectId(projectId, [
    { tag: 'ProjectTitle', contents: projectName }
  ]);
  return projectId
}


export default ({ isOpen, close }) => {
  const navigate = useNavigate();
  const [projectName, setProjectName] = useState('')
  const submit = useCallback(async () => {
    const projectId = await createProject(projectName);
    navigate(`/projects/${projectId}`);
  }, [projectName]);

  return (
    <Modal isOpen={isOpen} close={close}>
      <Root>
        <Title>New Project Model</Title>
        <Spacer height="10" />
        <Content>
         <ProjectName>Project Name</ProjectName>
         <InlineEdit
           onSave={setProjectName}
         />
        </Content>
        <Footer>
          <button onClick={close}>Cancel</button>
          <button
            disabled={!projectName}
            onClick={submit}
          >
            Submit
          </button>
        </Footer>
      </Root>
    </Modal>
  )
}

