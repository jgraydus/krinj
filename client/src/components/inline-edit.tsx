import { useCallback, useEffect, useRef, useState } from 'react'
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
const EditRoot = styled.input`
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
const Edit = ({ save, value, setValue }: any) => {
  const ref: any = useRef(null);

  const keypressHandler = useCallback((evt: KeyboardEvent) => {
      if (ref && ref.current && ref.current === evt.target && evt.key === "Enter") {
          save(value);
      }
  }, [save, value]);

  useEffect(() => {
      document.addEventListener('keypress', keypressHandler, false);
      return () => { document.removeEventListener('keypress', keypressHandler, false); }
  }, [keypressHandler]);

  return <EditRoot
            autoFocus
            ref={ref}
            value={value}
            onChange={e => setValue(e.target.value)}
            onBlur={() => save(value)} />
}


interface InlineEditParams {
    initialValue?: string,
    onSave?: (onSave: string) => any,
}

export default ({ 
    initialValue = '',
    onSave,
}: InlineEditParams) => {
    const [mode, setMode] = useState(InlineEditMode.VIEW);
    const [value, setValue] = useState(initialValue);

    const toggle = useCallback(() => setMode(flip(mode)), [mode]);

    const save = useCallback((val: string) => {
        setValue(val);
        if (onSave) { onSave(val); }
        toggle();
    }, [onSave, setValue, toggle]);

    return (
      <Root>
        {mode === InlineEditMode.VIEW
          ? <View onClick={toggle}>{value}</View>
          : <Edit save={save} setValue={setValue} value={value} />}
      </Root>
    )
}

