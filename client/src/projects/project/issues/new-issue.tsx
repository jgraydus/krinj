import { useCallback, useState } from 'react'
import { useDispatch } from 'react-redux'
import styled from 'styled-components'
import api from '../../../api'
import Modal from '../../../components/modal'
import InlineEdit from '../../../components/inline-edit'
import { createIssue } from '../../../redux'

const Root = styled.div`
  height: 100%;
  width: 100%;
  display: flex;
  flex-flow: column nowrap;
`
const Title = styled(({ className }) =>
  <div className={className}>New Issue</div>
)`
  font-size: 20px;
`
const Content = styled.div`
  height: 0px;
  flex-grow: 1;
  flex-shrink: 1;
`
const Footer = styled.div`
  display: flex;
  flex-flow: row nowrap;
  justify-content: flex-end;
`

export default ({ close, isOpen, projectId }) => {
  const dispatch = useDispatch();
  const [name, setName] = useState('');

  const newIssue = useCallback(async () => {
    dispatch(createIssue(projectId, name));
    close()
  }, [name, projectId]);

  return (
    <Modal isOpen={isOpen} close={close}>
      <Root>
        <Title />
        <Content>
          <div>
            <div>Issue name</div>
            <InlineEdit onSave={setName} />
          </div>
        </Content>
        <Footer>
          <button onClick={close}>Cancel</button>
          <button
            disabled={name.length === 0}
            onClick={newIssue}
          >
            Submit
          </button>
        </Footer>
      </Root>
    </Modal>
  )
}

