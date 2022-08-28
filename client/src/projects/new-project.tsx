import styled from 'styled-components'
import Modal from '../components/modal'

const Root = styled.div`
  width: 1024px;
  height: 80%;
`


export default ({ isOpen, close }) => {

  return (
    <Modal isOpen={isOpen} close={close}>
      <Root onClick={close}>New Project Model</Root>
    </Modal>
  )
}

