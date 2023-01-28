import styled from 'styled-components'

export default styled.div<{ width?: number, height?: number }>`
  width: ${props => props.width || 0}px;
  height: ${props => props.height || 0}px;
`

