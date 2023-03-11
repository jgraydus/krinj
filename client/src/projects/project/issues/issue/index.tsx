import * as R from 'ramda'
import { useCallback, useEffect } from 'react'
import { useNavigate, useParams } from 'react-router-dom'
import styled from 'styled-components'

import InlineEdit from '../../../../components/inline-edit'
import Loading from '../../../../components/loading'
import MdEditor from '../../../../components/md-editor'
import { loadEntity, selectEntity, updateAttribute, useDispatch, useSelector } from '../../../../redux'

const Root = styled.div`
  box-sizing: border-box;
  height: 100%;
  width: 100%;
  display: flex;
  flex-flow: column nowrap;
  padding: 5px;
`
const Row = styled.div`
  width: 100%;
  display: flex;
  flex-flow: row nowrap;
  align-items: center;
  padding-top: 1px;
  padding-bottom: 1px;
`
const BigRow = styled.div`
  width: 100%;
  flex-grow: 1;
  display: flex;
  flex-flow: row nowrap;
  align-items: center;
  padding-top: 1px;
  padding-bottom: 1px;
`
const Label = styled.div`
  width: 15%;
  padding-top: 5px;
  padding-bottom: 5px;
  display: flex;
  flex-flow: row nowrap;
`
const Cell = styled.div`
  width: 85%;
  display: flex;
  flex-flow: row nowrap;
`
const BigCell = styled.div`
  width: 85%;
  height: 100%;
  flex-grow: 1;
  flex-shrink: 1;
  display: flex;
  flex-flow: row nowrap;
`
const BackButton = styled(({ className, onClick }) =>
  <div className={className}>
    <button onClick={onClick}>Back</button>
  </div>
)`
  padding-bottom: 5px;
`

export default () => {
  const dispatch = useDispatch();
  const navigate = useNavigate();
  const { entityId, projectId }: any = useParams()
  const issue: Entity | null = useSelector(state => selectEntity(state, entityId))

  useEffect(() => {
    dispatch(loadEntity(entityId));
  }, [entityId]);

  const back = useCallback(() => {
    navigate(`/projects/${projectId}/issues`);
  }, [projectId]);

  const updateName = useCallback((name: string) => {
    dispatch(updateAttribute(entityId, { attributeName: 'name', attributeValue: name }));
  }, [entityId]);

  const updateDescription = useCallback((desc: string) => {
    dispatch(updateAttribute(entityId, { attributeName: 'description', attributeValue: desc }));
  }, [entityId]);

  return issue ? (
    <Root>
      <Row>
        <BackButton onClick={back} />
      </Row>

      <Row>
        <Label>Name</Label>
        <Cell>
         <InlineEdit
           initialValue={R.pathOr('--', ['attributes', 'name'], issue)}
           onSave={updateName} />
        </Cell>
      </Row>

      <Row>
        <Label>State</Label>
        <Cell>
          <InlineEdit
            initialValue={R.pathOr('--', ['attributes', 'state'], issue)}
            onSave={console.log}/>
        </Cell>
      </Row>

      <Row>
        <Label>Assignee</Label>
        <Cell>
          <InlineEdit
            initialValue={R.pathOr('--', ['attributes', 'assignee'], issue)}
            onSave={console.log}/>
        </Cell>
      </Row>

      <BigRow>
        <Label>Description</Label>
        <BigCell>
          <MdEditor
            initialValue={R.pathOr('--', ['attributes', 'description'], issue)}
            onSave={updateDescription} />
        </BigCell>
      </BigRow>

      <Row>
        <Label>Owner</Label>
        <Cell>{R.pathOr('--', ['attributes', 'owner'], issue)}</Cell>
      </Row>

      <Row>
        <Label>Created</Label>
        <Cell>{R.pathOr('--', ['attributes', 'createdAt'], issue)}</Cell>
      </Row>
      
      <Row>
        <Label>Last Updated</Label>
        <Cell>{R.pathOr('--', ['attributes', 'modifiedAt'], issue)}</Cell>
      </Row>
    </Root>
  ) : <Loading />
}

