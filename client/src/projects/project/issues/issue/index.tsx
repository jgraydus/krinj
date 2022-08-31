import { useParams } from 'react-router-dom'
import styled from 'styled-components'



export default () => {
  const { issueId, projectId } = useParams()

  return (
    <div>ISSUES projectId={projectId} issueId={issueId}</div>
  )
}

