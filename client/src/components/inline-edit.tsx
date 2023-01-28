import { useCallback, useState } from 'react'
import styled from 'styled-components'

const enum InlineEditMode { VIEW, EDIT }

const flip = (mode: InlineEditMode) => mode === InlineEditMode.VIEW
  ? InlineEditMode.EDIT
  : InlineEditMode.VIEW;

const Root = styled.div`
  display: inline-block;
  box-sizing: border-box;
  width: 100%;
  height: 30px;
`
const View = styled.div`
  box-sizing: border-box;
  width: 100%;
  height: 100%;
  display: flex;
  align-items: center;
  justify-content: flex-start;
  padding: 5px;
  font-size: 12px;
  font-style: normal;
  font-weight: normal;
  font-kerning: normal;
  font-variant: normal;
  font-family: "Lucida", sans-serif;
  letter-spacing: 0px;
  border: 1px solid black;
`
const Edit = styled.input`
  box-sizing: border-box;
  width: 100%;
  height: 100%;
  padding: 5px;
  font-size: 12px;
  font-style: normal;
  font-weight: normal;
  font-kerning: normal;
  font-variant: normal;
  font-family: "Lucida", sans-serif;
  letter-spacing: 0px;
  border: 1px solid black;
  outline: none;
`

export default ({ initialValue, onSave }: { initialValue?: string, onSave: any }) => {
  const [mode, setMode] = useState(InlineEditMode.VIEW);
  const [value, setValue] = useState(initialValue || '');

  const toggle = useCallback(() => setMode(flip(mode)), [mode, setMode]);
  const onChange: any = useCallback((e: any) => setValue(e.target.value), [setValue]);
  const save = useCallback(() => { onSave(value); toggle(); }, [onSave, toggle, value]);

  return (
    <Root>
      {mode === InlineEditMode.VIEW
        ? <View onClick={toggle}>{value}</View>
        : <Edit value={value} onChange={onChange} autoFocus onBlur={save} />}
    </Root>
  )
}
