import * as R from 'ramda'
import { useCallback, useState } from 'react'
import styled from 'styled-components'
import { Button, InlineEdit, Modal } from 'components'
import { createEntity, useDispatch } from 'data'

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

export default ({ close, isOpen, project }: { close: any, isOpen: boolean, project: Project }) => {
  const dispatch = useDispatch();
  const [name, setName] = useState('');
  const [entityType, setEntityType] = useState(project.entityTypes[0]);

  if (!entityType) {
    throw new Error('entityType is missing');
  }

  const newIssue = useCallback(async () => {
    const args = {
        entityTypeId: entityType.entityTypeId,
        attributes: {
            name,
            state: 'OPEN',
        }
    };
    dispatch(createEntity(project.projectId, args));
    close()
  }, [name, project.projectId]);

  return (
    <Modal isOpen={isOpen} close={close}>
      <Root>
        <Title />
        <Content>
          <div>
            <div>Issue name</div>
            <InlineEdit onSave={setName} />
          </div>
          <select
            value={entityType.name}
            onChange={(evt) => {
              const value = evt.target.value;
              const entityType = R.find(R.propEq('name', value), project.entityTypes);
              if (entityType) {
                  setEntityType(entityType);
              }
            }}
          >
            {R.map(
                (entityType: EntityType) => <option key={entityType.entityTypeId}>{entityType.name}</option>,
                project.entityTypes
            )}
          </select>
        </Content>
        <Footer>
          <Button onClick={close}>Cancel</Button>
          <Button
            disabled={name.length === 0}
            onClick={newIssue}
          >
            Submit
          </Button>
        </Footer>
      </Root>
    </Modal>
  )
}

