import styled from 'styled-components'

const Input = styled.input`
    width: 100%;
    height: 30px;
    border: 1px solid black;
    border-radius: 6px;
    background-color: white;
    color: black !important;
    &:focus { outline: thin dashed black; }
    content: 'foobarbaz';
`
const Label = styled.label`
    width: 100%;
    font-size: 10px;
    line-height: 15px;
    color black;
`

const Component = (props: {
    label?: string,
    value?: string,
    onChange?: (evt: React.ChangeEvent<HTMLInputElement>) => void,
    type?: "text" | "password"
}) => {
    const { label, value, onChange, type = "text" } = props;
    const hasLabel = !!label;

    return hasLabel ? (
        <Label>
           <Input value={value} onChange={onChange} type={type} />
           {label} 
        </Label>
    ) : (
        <Input value={value} onChange={onChange} type={type} />
    )
}

export default Component;

