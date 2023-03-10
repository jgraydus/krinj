import * as R from 'ramda'
import { useCallback, useState } from 'react'
import styled from 'styled-components'
import api from '../../../api'
import Modal from '../../../components/modal'
import InlineEdit from '../../../components/inline-edit'
import { createEntity, useDispatch } from '../../../redux'

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

  const newIssue = useCallback(async () => {
    const args = { entityTypeId: entityType.entityTypeId, attributes: { name } };
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

