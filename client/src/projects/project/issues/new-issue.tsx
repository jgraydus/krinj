import styled from 'styled-components'
import Modal from '../../../components/modal'

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

  return (
    <Modal isOpen={isOpen} close={close}>
      <Root>
        <Title />
        <Content>
        stuff here
        </Content>
        <Footer>
          <button onClick={close}>Cancel</button>
          <button>Submit</button>
        </Footer>
      </Root>
    </Modal>
  )
}

