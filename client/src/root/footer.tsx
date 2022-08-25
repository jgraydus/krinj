import styled from 'styled-components'

const Root = styled.div`
  font-size: 10px;
  color: #333;
  padding: 5px;
  display: flex;
  align-items: center;
  justify-content: flex-end;
`

export default () => <Root>{window.IssueTracker.commitHash}</Root>

