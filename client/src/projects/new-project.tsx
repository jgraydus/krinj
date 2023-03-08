import { useCallback, useState } from 'react'
import { useNavigate } from 'react-router-dom'
import styled from 'styled-components'
import InlineEdit from '../components/inline-edit'
import Modal from '../components/modal'
import Spacer from '../components/spacer'
import { createProject, useDispatch } from '../redux'

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
  width: 100%;
  height: 0px;
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

export default ({ isOpen, close }: { isOpen: boolean, close: any }) => {
  const dispatch = useDispatch();
  const navigate = useNavigate();
  const [projectName, setProjectName] = useState('')

  const submit = useCallback(async () => {
    const projectId = await dispatch(createProject({ projectName, projectDescription: '' }));
    navigate(`/projects/${projectId}`);
  }, [projectName]);

  return (
    <Modal isOpen={isOpen} close={close}>
      <Root>
        <Title>New Project</Title>
        <Spacer height={10} />
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

