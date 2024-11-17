import clsx from 'clsx';
import Heading from '@theme/Heading';
import styles from './styles.module.css';

type FeatureItem = {
  path: string;
  title: string;
  description: string;
};
const FeatureList: FeatureItem[] = [];

function Feature({path, title, description}: FeatureItem) {
  return (
    <div className={clsx('col col--4')}>
      <div className={`${styles.card} text--center padding-horiz--md`}>
        <Link to="/{path}">
          <Heading as="h3">{title}</Heading>
          <p>{description}</p>
        </Link>
      </div>
    </div>
  );
}

export default function HomepageFeatures(): JSX.Element {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}
