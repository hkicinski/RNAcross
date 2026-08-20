// RNAcross study-design upload wizard (reactR Shiny input).
// R feeds configuration.columns + column_values + column_numeric; the component
// emits a `spec` via setValue() into input$<inputId>, validated by R/13's
// wizard_spec_to_design(). Steps so far: Compare-across, Primary-axis.
// (Samples + Review + a stepper wrapper come next.)
import React from 'react';
import { reactShinyInput } from 'reactR';

const S = {
  wrap: { padding: 12, fontFamily: 'inherit', maxWidth: 640 },
  section: { marginBottom: 24 },
  head: { fontSize: 15, fontWeight: 700, borderBottom: '2px solid #e5e7eb',
          paddingBottom: 4, marginBottom: 12 },
  field: { marginBottom: 16 },
  label: { display: 'block', fontWeight: 600, marginBottom: 4 },
  hint: { fontWeight: 400, color: '#6b7280', fontSize: 12 },
  typeHint: { color: '#6b7280', fontSize: 12, marginTop: 4, maxWidth: 480 },
  select: { padding: '4px 6px', minWidth: 260 },
  text: { padding: '4px 6px', minWidth: 200 },
  radio: { fontWeight: 400, marginRight: 18 },
  check: { display: 'block', fontWeight: 400, padding: '1px 0' },
  row: { display: 'flex', alignItems: 'center', gap: 8, padding: '3px 0' },
  btn: { border: '1px solid #ccc', background: '#f7f7f7', borderRadius: 4,
         cursor: 'pointer', width: 26, height: 24 },
  btnOff: { opacity: 0.3, cursor: 'default' },
  name: { minWidth: 96 },
  num: { width: 84, padding: '2px 4px' },
};

const Section = ({ n, title, children }) => (
  <div style={S.section}>
    <div style={S.head}>{n}. {title}</div>
    {children}
  </div>
);

const TYPES = [
  { key: 'nominal', label: 'Separate groups (unordered)',
    hint: 'Distinct categories with no natural order (genotypes, tissues, conditions).' },
  { key: 'ordinal', label: 'Ordered stages',
    hint: 'Ranked stages where order matters but the spacing is not numeric (egg, larva, pupa, adult).' },
  { key: 'interval', label: 'Numeric progression (timecourse, dose)',
    hint: 'Numeric points along a scale. Spacing is inferred from the labels below; edit any value to override.' },
];

function StudyDesignWizard({ configuration, value, setValue }) {
  const cfg = configuration || {};
  const columns = cfg.columns || [];
  const columnValues = cfg.column_values || {};
  const columnNumeric = cfg.column_numeric || {};
  const spec = value || {};
  const update = (patch) => setValue({ ...spec, ...patch });

  // ---------- Compare across ----------
  const align = spec.align || 'identity';
  const genomeCol = spec.genome_col || '';
  const members = spec.members || [];
  const memberOptions = columnValues[genomeCol] || [];
  const membersPhrase = members.length === 0 ? 'the members'
    : members.length <= 3 ? members.join(', ')
    : `${members.slice(0, 3).join(', ')}, +${members.length - 3} more`;

  const setAlign = (a) =>
    a === 'orthology'
      ? update({ align: 'orthology', members: [], comparison_label: spec.comparison_label || 'Species' })
      : update({ align: 'identity', genome_col: undefined, members: ['genome'], comparison_label: 'Sample' });

  const chooseGenomeCol = (col) =>
    update({
      align: 'orthology',
      genome_col: col || undefined,
      members: (columnValues[col] || []).slice(),
      comparison_label: col || 'Species', // the column name is usually the collective noun
    });

  const toggleMember = (m) => {
    const has = members.includes(m);
    update({ members: memberOptions.filter((x) => (x === m ? !has : members.includes(x))) });
  };

  // ---------- Primary axis ----------
  const type = spec.condition_type || 'nominal';
  const levels = spec.condition_levels || [];
  const values = spec.condition_values || [];
  const ordered = type !== 'nominal';

  const numericDefaults = (col, lv) => {
    const inf = columnNumeric[col];
    if (Array.isArray(inf) && inf.length === lv.length) return inf.map(String);
    return lv.map((_, i) => String(i + 1));
  };

  const chooseColumn = (col) => {
    const lv = (columnValues[col] || []).slice();
    const inf = columnNumeric[col];
    // auto-detect: a numeric / time-like column -> interval (with inferred spacing),
    // otherwise categorical. always writes condition_type so the spec is complete.
    const t = (Array.isArray(inf) && inf.length === lv.length && lv.length > 0) ? 'interval' : 'nominal';
    update({
      condition_column: col || undefined,
      condition_type: t,
      condition_levels: lv,
      condition_reference: undefined,
      condition_values: t === 'interval' ? numericDefaults(col, lv) : undefined,
    });
  };

  const setType = (t) =>
    update({
      condition_type: t,
      condition_values: t === 'interval' ? numericDefaults(spec.condition_column, levels) : undefined,
    });

  const move = (i, dir) => {
    const j = i + dir;
    if (j < 0 || j >= levels.length) return;
    const lv = levels.slice(); [lv[i], lv[j]] = [lv[j], lv[i]];
    const patch = { condition_levels: lv };
    if (type === 'interval') {
      const v = values.slice(); [v[i], v[j]] = [v[j], v[i]];
      patch.condition_values = v;
    }
    update(patch);
  };

  const setNum = (i, val) => {
    const v = levels.map((_, k) => values[k] ?? '');
    v[i] = val;
    update({ condition_values: v });
  };

  const typeHint = (TYPES.find((t) => t.key === type) || {}).hint;

  return (
    <div style={S.wrap}>
      <Section n="1" title="Compare across">
        <div style={S.field}>
          <label style={S.label}>Are all samples the same genome, or different species / strains?</label>
          <label style={S.radio}>
            <input type="radio" name="align" checked={align === 'identity'}
                   onChange={() => setAlign('identity')} /> Same genome (one species / strain)
          </label>
          <label style={S.radio}>
            <input type="radio" name="align" checked={align === 'orthology'}
                   onChange={() => setAlign('orthology')} /> Different species / strains (compare via orthology)
          </label>
        </div>

        {align === 'orthology' ? (
          <div>
            <div style={S.field}>
              <label style={S.label}>Which column labels the species / strain?</label>
              <select style={S.select} value={genomeCol}
                      onChange={(e) => chooseGenomeCol(e.target.value)}>
                <option value="">-- choose a column --</option>
                {columns.map((c) => <option key={c} value={c}>{c}</option>)}
              </select>
            </div>

            {genomeCol && memberOptions.length > 0 && (
              <div>
                <div style={S.field}>
                  <label style={S.label}>Members <span style={S.hint}>(which to include)</span></label>
                  {memberOptions.map((m) => (
                    <label key={m} style={S.check}>
                      <input type="checkbox" checked={members.includes(m)}
                             onChange={() => toggleMember(m)} /> {m}
                    </label>
                  ))}
                </div>

                <div style={S.field}>
                  <label style={S.label}>What are the members, collectively?</label>
                  <div style={{ display: 'flex', alignItems: 'center', gap: 8, flexWrap: 'wrap' }}>
                    <span style={{ color: '#374151' }}>{membersPhrase} are my</span>
                    <input type="text" style={S.text} value={spec.comparison_label || ''}
                           placeholder="Species" onChange={(e) => update({ comparison_label: e.target.value })} />
                  </div>
                  <div style={S.hint}>the collective name, shown on plots and legends (Species, Strains, Isolates, Ecotypes...)</div>
                </div>
              </div>
            )}
          </div>
        ) : (
          <div style={S.typeHint}>All samples are one genome, genes are matched directly (no orthology mapping).</div>
        )}
      </Section>

      <Section n="2" title="Primary axis">
        <div style={S.field}>
          <label style={S.label}>Which column is the primary axis?</label>
          <select style={S.select} value={spec.condition_column || ''}
                  onChange={(e) => chooseColumn(e.target.value)}>
            <option value="">-- choose a column --</option>
            {columns.map((c) => <option key={c} value={c}>{c}</option>)}
          </select>
        </div>

        <div style={S.field}>
          <label style={S.label}>How are the levels related?</label>
          <select style={S.select} value={type} onChange={(e) => setType(e.target.value)}>
            {TYPES.map((t) => <option key={t.key} value={t.key}>{t.label}</option>)}
          </select>
          <div style={S.typeHint}>{typeHint}</div>
        </div>

        {levels.length > 0 && (
          <div style={S.field}>
            <label style={S.label}>
              Levels{' '}
              <span style={S.hint}>
                {ordered ? '- arrange in order (top = first)' : '- order does not matter'}
                {type === 'interval' ? ', numbers inferred from labels (edit to override)' : ''}
              </span>
            </label>

            {levels.map((lv, i) => (
              <div key={lv} style={S.row}>
                {ordered && (
                  <span>
                    <button style={{ ...S.btn, ...(i === 0 ? S.btnOff : {}) }}
                            onClick={() => move(i, -1)} title="move up">↑</button>
                    <button style={{ ...S.btn, ...(i === levels.length - 1 ? S.btnOff : {}) }}
                            onClick={() => move(i, 1)} title="move down">↓</button>
                  </span>
                )}
                <span style={S.name}>{lv}</span>
                {type === 'interval' && (
                  <input type="number" style={S.num} value={values[i] ?? ''}
                         placeholder="0" onChange={(e) => setNum(i, e.target.value)} />
                )}
              </div>
            ))}

            <div style={{ marginTop: 12 }}>
              <label style={S.label}>
                Reference / baseline <span style={S.hint}>(for fold-change; optional)</span>
              </label>
              <select style={S.select} value={spec.condition_reference || ''}
                      onChange={(e) => update({ condition_reference: e.target.value || undefined })}>
                <option value="">(none)</option>
                {levels.map((lv) => <option key={lv} value={lv}>{lv}</option>)}
              </select>
            </div>
          </div>
        )}
      </Section>

      <Section n="3" title="Samples">
        <div style={S.field}>
          <label style={S.label}>
            Replicate column <span style={S.hint}>(optional, groups repeats of the same condition)</span>
          </label>
          <select style={S.select} value={spec.replicate || ''}
                  onChange={(e) => update({ replicate: e.target.value || undefined })}>
            <option value="">(none)</option>
            {columns.map((c) => <option key={c} value={c}>{c}</option>)}
          </select>
        </div>
      </Section>
    </div>
  );
}

reactShinyInput('.study_design_wizard', 'RNAcross.studyDesignWizard', StudyDesignWizard);
