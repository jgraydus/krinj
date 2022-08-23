import styled from 'styled-components'

import Header from './page-header'

const FooterStyles = styled.div`
  padding: 5px;
`
const Footer = () => <FooterStyles>FOOTER</FooterStyles>

const Root = styled.div`
  position: absolute;
  top: 0;
  bottom: 0;
  left: 0;
  right: 0;

  padding: 0 10px 0 10px;

  background-color: #AABBBB;
  display: flex;
  justify-content: center;

  font-family: "Lucida", sans-serif;
`

const ContentArea = styled.div`
  width: 1024px;
  border: 1px solid black;
  background-color: #BFCFCF;
  display: flex;
  flex-flow: column nowrap;
  align-items: stretch;
`

const Content = styled.div`
  display: flex;
  flex-flow: column nowrap;
  flex-grow: 1;
  border: 1px solid red;
`

export default ({ children }) =>
  <Root>
    <ContentArea>
      <Header />
        <Content>{children}</Content>
      <Footer />
    </ContentArea>
  </Root>

