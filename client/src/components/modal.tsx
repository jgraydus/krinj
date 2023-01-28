import { ReactNode } from 'react'
import ReactModal from 'react-modal'
import styled from 'styled-components'

const rootElement = document.getElementById('root')

if (rootElement !== null) {
    ReactModal.setAppElement(rootElement);
} else {
    throw new Error("failed to find root element")
}

const style: any = {
  content: {
    position: 'relative',
    top: 0, left: 0, bottom: 0, right: 0,
    width: '1096px',
    height: '80%',
    padding: '10px',
    margin: 0,
    backgroundColor: '#BFCFCF',
    boxSizing: 'border-box',
    fontFamily: '"Lucida", sans-serif'
  },
  overlay: {
    display: 'flex',
    justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: 'rgba(0,0,0,0.5)'
  }
}

const CloseButton = styled.div`
  height: 20px;
  width: 20px;
  position: absolute;
  top: 5px;
  right: 5px;
  border: 1px solid black;
  display: flex;
  justify-content: center;
  alignt-items: center;
  font-size: 20px;
  line-height: 20px;
  cursor: pointer;
  border-radius: 3px;
`
const Content = styled.div`
  box-sizing: border-box;
  height: 100%;
  width: 100%;
`

export default ({
    children, close, isOpen
}: {
    children: ReactNode, close: () => void, isOpen: boolean
}) =>
  <ReactModal isOpen={isOpen} style={style}>
    <CloseButton onClick={close}>X</CloseButton>
    <Content>
      {children}
    </Content>
  </ReactModal>

