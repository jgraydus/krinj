declare const Component: (props: {
    label?: string;
    value?: string;
    onChange?: (evt: React.ChangeEvent<HTMLInputElement>) => void;
    type?: "text" | "password";
}) => JSX.Element;
export default Component;
